# Title: Preparation of indcators for woa survey
# Authors: Sayed Nabizada, Christopher Jarvis, 
# Date created: 21/09/2019
# Date last changed: 21/09/2019
# Purpose: This script is for recoding the indicators in the whole of 
#          of Afghanistan survey data
#          disaggregations and composite scores are created. 

# setup analysis environment
source("./R/source.R")

# character operation
ch<-as.character
chr<-as.character

coerc<-function(x){as.numeric(chr(x))}

# load data 
data <- read_excel(master_data, sheet = "MSNA_AFG_19_parent_sheet", na = c("","NA"))



## Grouping indicators ####






## Need to group these into place


# Adjust displacement status
non_displ_data <- read.csv("input/Non_Displaced_List.csv",stringsAsFactors=F,na.strings = c("", "NA"))
data<-full_join(data, non_displ_data,by = c("district"="district"))
data$final_displacement_status_non_displ<-ifelse(data$final_displacement_status=='non_displaced'|data$final_displacement_status=='host', data$non_displ_class,data$final_displacement_status)


#Recoding new variables
data$hoh_age_group <- ifelse(data$hoh_age >= 60, "60+", 
                             ifelse(data$hoh_age <= 60 & data$hoh_age > 17,"18-59",
                                    ifelse(data$hoh_age <= 17 & data$hoh_age > 6, "6-17","0-5")))

data$hh_no_tazkira <- ifelse(data$tazkira_total < 1, "Tazkira_No", "Tazkira_Yes")

data$muac_yes_no <- ifelse(data$muac_total > 0 & !is.na(data$min_muac)  ,"Yes","No")



data$all_expenses <- coerc(data[["food_exp"]]) + coerc(data[["water_expt"]]) + coerc(data[["rent_exp"]]) + coerc(data[["fuel_exp"]]) + coerc(data[["debt_exp"]])

data$food_water_rent_com <- ifelse(data$total_income > 0, (coerc(data[["food_exp"]]) + coerc(data[["water_expt"]]) + coerc(data[["rent_exp"]]) / coerc(data[["total_income"]])), data$total_income)


data$food_exp_cal <- ifelse(data$total_income > 0, (coerc(data[["food_exp"]]) / coerc(data[["total_income"]])), data$total_income)
data$water_expt_cal <- ifelse(data$total_income > 0, (coerc(data[["water_expt"]]) / coerc(data[["total_income"]])), data$total_income)
data$rent_exp_cal <- ifelse(data$total_income > 0, (coerc(data[["rent_exp"]]) / coerc(data[["total_income"]])), data$total_income)
data$fuel_exp_cal <- ifelse(data$total_income > 0, (coerc(data[["fuel_exp"]]) / coerc(data[["total_income"]])), data$total_income)
data$debt_exp_cal <- ifelse(data$total_income > 0, (coerc(data[["debt_exp"]]) / coerc(data[["total_income"]])), data$total_income)





data$pregnant_member <- ifelse(data$pregnant > 0, "at_least_one_mem_pregnant", "no_mem_pregnent")
data$lactating_member <- ifelse(data$lactating > 0, "at_least_one_mem_lactating", "no_mem_lactating")

data$ag_income_cal <- ifelse(data$ag_income > 0, data$ag_income / data$hh_size, data$ag_income)
data$livestock_income_cal <- ifelse(data$livestock_income > 0, data$livestock_income / data$hh_size, data$livestock_income)
data$rent_income_cal <- ifelse(data$rent_income > 0, data$rent_income / data$hh_size, data$rent_income )
data$small_business_income_cal <- ifelse(data$small_business_income > 0, data$small_business_income / data$hh_size, data$small_business_income )
data$unskill_labor_income_cal <- ifelse(data$unskill_labor_income > 0, data$unskill_labor_income / data$hh_size, data$unskill_labor_income )
data$skill_labor_income_cal <- ifelse(data$skill_labor_income > 0, data$skill_labor_income / data$hh_size, data$skill_labor_income )
data$formal_employment_income_cal <- ifelse(data$formal_employment_income > 0, data$formal_employment_income / data$hh_size, data$formal_employment_income )
data$gov_benefits_income_cal <- ifelse(data$gov_benefits_income > 0, data$gov_benefits_income / data$hh_size, data$gov_benefits_income)
data$hum_assistance_income_cal <- ifelse(data$hum_assistance_income > 0, data$hum_assistance_income / data$hh_size, data$hum_assistance_income)
data$remittance_income_cal <- ifelse(data$remittance_income > 0, data$remittance_income / data$hh_size, data$remittance_income)
data$loans_income_cal <- ifelse(data$loans_income > 0, data$loans_income / data$hh_size, data$loans_income)
data$asset_selling_income_cal <- ifelse(data$asset_selling_income > 0, data$asset_selling_income / data$hh_size, data$asset_selling_income)
data$total_income_cal <- ifelse(data$total_income > 0, data$total_income / data$hh_size, data$total_income)


data$recent_non_recent <- ifelse(data$final_displacement_status_non_displ == "recent_idps", "recent_idps",
                                 ifelse(data$final_displacement_status_non_displ == "non_recent_idps", "non_recent_idps", NA ))
data$edu_removal_shock_cal <-  ifelse(data$shock_class == 5, "Yes", "No") 

data$enrolled_attending <- ifelse(data$count_enrolled_attending > 0, "Enrolled_and_Attending", "Not" ) 

data$female_literacy_yes_no <- ifelse(data$female_literacy < 1, "0", "1 or more")
data$male_literacy_yes_no <- ifelse(data$male_literacy < 1, "0", "1 or more")



data$edu_age_boys_girls <-  coerc(data[["boys_ed"]]) + coerc(data[["girls_ed"]])
data$count_current_enrolled_avg <- coerc(data[["count_current_enrolled"]]) / data$edu_age_boys_girls
data$count_current_attending_avg <- coerc(data[["count_current_attending"]]) / data$edu_age_boys_girls


data <- data %>% 
  mutate(
    prev_displacement_num_class = case_when(
      prev_displacement_num == 2 ~ "2",
      prev_displacement_num == 3 ~ "3",
      prev_displacement_num >3 ~ "4+"
    ),
    
    refugee_displace_year_class = case_when(
      refugee_displace_year == 0 ~ "0",
      refugee_displace_year == 1 ~ "1",
      refugee_displace_year == 2 ~ "2",
      refugee_displace_year == 3 ~ "3",
      refugee_displace_year > 3 ~ "4+"
    ),
    
    cb_return_displace_year_class = case_when(
      cb_return_displace_year == 0 ~ "0",
      cb_return_displace_year == 1 ~ "1",
      cb_return_displace_year == 2 ~ "2",
      cb_return_displace_year == 3 ~ "3",
      cb_return_displace_year > 3 ~ "4+"
    ),
    
    cb_return_return_year_call = case_when(
      cb_return_return_year == 0 ~ "0",
      cb_return_return_year == 1 ~ "1",
      cb_return_return_year == 2 ~ "2",
      cb_return_return_year == 3 ~ "3",
      cb_return_return_year > 3 ~ "4+"
    ),
    
    idp_displ_year_class = case_when(
      idp_displ_year == 0 ~ "0",
      idp_displ_year == 1 ~ "1",
      idp_displ_year == 2 ~ "2",
      idp_displ_year == 3 ~ "3",
      idp_displ_year > 3 ~ "4+"
    )
    
  )


write.csv(data, "./input/data/recoded/data_with_strata.csv", row.names = F)
