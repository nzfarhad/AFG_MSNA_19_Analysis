
library(lubridate)
library(readxl)
library(openxlsx)
# source("R/woa_tracking_deletion/function.R")
library(tidyverse)
library(cleaninginspectoR)

#load cleaning log - prev round of data cleaning
cleaning_log <- read_excel("data_cleaning_assets/input/cleaning_log_prev_round_of_cleaning.xlsx") %>%
  filter(question != "muac_measurement")


# Poplulation group check
sample_checker <-read_csv("data_cleaning_assets/input/sample_checker.csv")

host_sample <- sample_checker %>% filter(pop_group == "host")
non_displaced_sample <- sample_checker %>% filter(pop_group %in% c("all_non_displaced", 
                                                                   "conflict_affected", 
                                                                   "natural_disaster_affected"))

#pilot and extra deletions uuids to remove from dataset
deletion_uuid <- read_excel("data_cleaning_assets/input/AFG1901_WoA_MSNA_uuid_deletion.xlsx")
# pattern check uuids to declare NA
fcs_declare_na <- read_excel("data_cleaning_assets/input/fcs_declare_na.xlsx")
muac_declare_na <- read_excel("data_cleaning_assets/input/muac_declare_na.xlsx")

#import msna data
overall_data <- read_excel("data_cleaning_assets/input/AFG1901_WoA_MSNA_v4.xlsx") %>%
  left_join(select(deletion_uuid, `_uuid`, data_cleaning_round), by = c("_uuid"="_uuid")) %>% 
  filter(is.na(data_cleaning_round)) %>% 
  type_convert() 

# sterilize dataset
overall_data <- select (overall_data,-c(follow_up_consent, respondent_telephone,
                                        geopoint, `_geopoint_latitude`, `_geopoint_longitude`,
                                        `_geopoint_altitude`, `_geopoint_precision`,
                                        data_cleaning_round))


# load muac loop
overall_muac_data <- read_excel("data_cleaning_assets/inputAFG1901_WoA_MSNA_v4.xlsx", sheet = "g1") %>%
  filter(person_muac == 1) %>%
  type_convert()

# muac na values columns
muac_na_vars <- c("person_muac",	"muac_measurement",
                  "moderate_malnutrition",	"severe_malnutrition",
                  "rutf_reception")
# muac na values rows
muac_na_rows <- overall_data$`_uuid` %in% muac_declare_na$uuid

# overwrite the values with NA as they are not trusted
overall_muac_data[muac_na_rows, muac_na_vars] <-  NA

# load other loops
overall_hh_roster <- read_excel("data_cleaning_assets/input/AFG1901_WoA_MSNA_v4.xlsx", sheet = "c1") %>% 
  type_convert()
overall_death_roster <- read_excel("data_cleaning_assets/input/AFG1901_WoA_MSNA_v4.xlsx", sheet = "i2") %>%
  type_convert()
overall_left_roster <- read_excel("data_cleaning_assets/input/AFG1901_WoA_MSNA_v4.xlsx", sheet = "i1") %>%
  type_convert()

 
# host and non-displaced districts
host_districts <- unique(host_sample$district_name)
non_displaced_districts <- unique(non_displaced_sample$district_name)
unique_host_districts <- host_districts[!(host_districts %in% non_displaced_districts)]
overlapping_host_districts <- host_districts[host_districts %in% non_displaced_districts]

host_settlements <- host_sample$settlement_name

# confirming population groups for surveys and flagging questionable ones

`%notin%` <- Negate(`%in%`)

overall_data <- overall_data %>%
  mutate(final_displacement_status = ifelse(displacement_status %in% "non_displaced" & district %in% non_displaced_districts, "non_displaced", displacement_status),
         final_displacement_status = ifelse(displacement_status %in% "non_displaced" & district %notin% non_displaced_districts, "non_displaced", final_displacement_status),
         final_displacement_status = ifelse(displacement_status %in% "non_displaced" & district %in% unique_host_districts, "host", final_displacement_status),
         final_displacement_status = ifelse(displacement_status %in% "non_displaced" & district %in% overlapping_host_districts, "host", final_displacement_status),
         final_displacement_status = ifelse(displacement_status %in% "non_displaced" & village %in% host_settlements & district %in% overlapping_host_districts, "host", final_displacement_status),
         final_displacement_status = ifelse(displacement_status %in% "idp", ifelse((idp_displace_hamal %in% "after_hamal" ) | (idp_displ_before_hamal %in% "less_2_months"), "recent_idps", "non_recent_idps"), final_displacement_status)) 


## load audit results
overall_time_audits <- read_csv("data_cleaning_assets/input/audit_results.csv")

# Deleted interviews
deleted_interviews<- overall_data %>%
  left_join(select(overall_time_audits, `_uuid`, duration_minutes), by = c("_uuid"="_uuid")) %>%
  filter(duration_minutes < 30 | food_consumption_score == 0)

deleted_yn <- overall_data %>%
  left_join(select(overall_time_audits, `_uuid`, duration_minutes), by = c("_uuid"="_uuid")) %>%
  mutate(deleted=ifelse(duration_minutes < 30 | food_consumption_score == 0, "deleted", "valid"))


# clean data - filter invalid interviews
overall_data_time_1 <- overall_data %>%
  left_join(select(overall_time_audits, `_uuid`, duration_minutes), by = c("_uuid"="_uuid")) %>% 
  filter(duration_minutes >= 30 & food_consumption_score > 0)


for (rowi in 1:nrow(cleaning_log)){
  uuid_i <- cleaning_log$uuid[rowi]
  var_i <- cleaning_log$question[rowi]
  old_i <- cleaning_log$`old value`[rowi]
  new_i <- cleaning_log$`new value`[rowi]
  # print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
  # Find the variable according to the row of the cleaning log
  overall_data_time_1[overall_data_time_1$`_uuid` == uuid_i, var_i] <- new_i
}


#######################################################################

## pattern check cleaning
# fcs na values columns
fcs_na_vars <- c("cereals_tubers",	"pulses_nuts",
                 "vegetables",	"fruit",
                 "meat_fish_eggs", "dairy",
                 "sugars",	"oils",
                 "food_consumption_score",	"fcs_category")
# fcs na values rows
fcs_na_rows <- overall_data$`_uuid` %in% fcs_declare_na$uuid
# overwrite the values with NA as they are not trusted
overall_data[fcs_na_rows, fcs_na_vars] <-  NA



overall_muac_data_1 <- overall_muac_data %>%
  left_join(select(overall_data_time_1, `_uuid`, region, province, survey_village, enumerator_uuid), by = c("_submission__uuid" = "_uuid")) %>%
  filter(enumerator_uuid != "NA")

overall_hh_roster_1 <- overall_hh_roster %>%
  left_join(select(overall_data_time_1, `_uuid`, region, province, survey_village, enumerator_uuid), by = c("_submission__uuid" = "_uuid")) %>%
  filter(enumerator_uuid != "NA")

overall_left_roster_1 <- overall_left_roster %>%
  left_join(select(overall_data_time_1, `_uuid`, region, province, survey_village, enumerator_uuid), by = c("_submission__uuid" = "_uuid")) %>%
  filter(enumerator_uuid != "NA")

overall_death_roster_1 <- overall_death_roster %>%
  left_join(select(overall_data_time_1, `_uuid`, region, province, survey_village, enumerator_uuid), by = c("_submission__uuid" = "_uuid")) %>%
  filter(enumerator_uuid != "NA")

# remove interviews druation column
overall_data_time_1 <- select (overall_data_time_1,-c(duration_minutes))

## output datasets
write_excel_csv(overall_data_time_1, paste0("data_cleaning_assets/output/MSNA_AFG_19_parent_sheet_",today(),".csv"))
write_excel_csv(overall_muac_data_1, paste0("data_cleaning_assets/output/MSNA_AFG_19_muac_",today() ,".csv"))
write_excel_csv(overall_hh_roster_1, paste0("data_cleaning_assets/output/MSNA_AFG_19_hh_roster_",today() ,".csv"))
write_excel_csv(overall_left_roster_1, paste0("data_cleaning_assets/output/MSNA_AFG_19_hh_left_roster_",today() ,".csv"))
write_excel_csv(overall_death_roster_1, paste0("data_cleaning_assets/output/MSNA_AFG_19_hh_death_roster_",today() ,".csv"))
write_excel_csv(deleted_interviews, paste0("data_cleaning_assets/output/Deleted_",today() ,".csv"))

# cleaning checks
cleaning_result <- (inspect_all(overall_data_time_1, uuid.column.name = "_uuid"))
write_excel_csv(cleaning_result, paste0("data_cleaning_assets/output/MSNA_AFG_19_cleaning_check", today() ,".csv"))
