# Title: Preparation of data for woa survey
# Authors: Sayed Nabizada, Christopher Jarvis, 
# Date created: 20/09/2019
# Date last changed: 21/09/2019
# Purpose: This script is for recoding variables in the whole of 
#          of Afghanistan survey data
#          indicators and composite scores are created. 

# setup analysis environment
source("./R/source.R")

# character operation
ch<-as.character
chr<-as.character

coerc<-function(x){as.numeric(chr(x))}

# load data 
data <- read_excel(master_data, sheet = "MSNA_AFG_19_parent_sheet", na = c("","NA"), guess_max = 3000)
overall_muac_data <- read_excel(master_data, sheet = "MSNA_AFG_19_muac" , na = c("","NA"))
overall_hh_roster <- read_excel(master_data, sheet = "MSNA_AFG_19_hh_roster" , na = c("","NA"))
overall_death_roster <- read_excel(master_data, sheet = "MSNA_AFG_19_hh_death_roster" , na = c("","NA"))
overall_left_roster <- read_excel( master_data, sheet = "MSNA_AFG_19_hh_left_roster" , na = c("","NA"))

# Temp for the data is exported out of kobo incorrectly. 
rename1 <- function(d1) {
  sub("/", ".", names(d1))
} 
names(data) <- rename1(data)
names(overall_muac_data ) <- rename1(overall_muac_data )
names(overall_hh_roster ) <- rename1(overall_hh_roster )
names(overall_death_roster ) <- rename1(overall_death_roster)
names(overall_left_roster ) <- rename1(overall_left_roster)


#  composite indicators #
# The composite indicators are a combination of different variables
# each value within a variable has a score and these need to be 
# coded for the different categories.
# Then the variables can be summed in order to get the score

# This will be done for multiple sectors.
#### Composite indicators ############
### Food Security & Agriculture ####

# FCS
data <- data %>%  
  mutate(
    # FCS
    fcs_category_class = recode(
      fcs_category,
      "poor" = 4,
      "borderline" = 2,
      "acceptable" = 0
      ),
    # HHS
    hhs_category_class = recode(
      hhs_category,
      "severe_hunger" = 4,
      "moderate_hunger" = 2,
      "little_hunger" = 0
      ),
    # Food Source
    food_source_class = case_when(
      food_source %in% c('gift', 'assistance') ~ 2, 
      food_source == 'borrowed' ~1,
      TRUE ~ 0
      ),
    # ag impact
    ag_impact_class = case_when(
      agricultural_impact_how == '76_100' ~ 3,
      agricultural_impact_how == '51_75' ~ 1,
      agricultural_impact %in% c('no', 'not_applicable') ~ 0,
      agricultural_impact_how %in% c('0_25', '26_50' ) ~ 0
      ),
    # livestock impact
    ls_impact_class = case_when(
      livestock_impact_how.livestock_died == 1 | 
        livestock_impact_how.left_unattended == 1 ~ 2, 
      livestock_impact_how.livestock_ill == 1 |
        livestock_impact_how.less_milk == 1 ~ 1,
      livestock_impact == 0 ~ 0,
      TRUE ~ 0,
      is.na(livestock_impact) ~ NA_real_
      )
    )

fsac_vars <- c("fcs_category_class", "hhs_category_class", "food_source_class", "ag_impact_class", "ls_impact_class")
data$fsac_score <- comp_score(data, fsac_vars)

data <- data %>% 
  mutate(
    fsac_severity = case_when(
      fsac_score <= 1 ~ 1, 
      fsac_score <= 3 ~ 2,
      fsac_score <= 7 ~ 3,
      fsac_score <= 16 ~ 4
    ),
    fsac_sev_high = case_when(
      fsac_severity <= 2 ~ 0,
      fsac_severity <= 4 ~ 1
      )
    )




##################################################################

### Protection ####

# First setup the variables required to calculate the indicators and then calculate them
# This way around if the weights are changed then it's all in one place.

# protection incidents

severe_prot_incidents_vars <-  c(
  "adult_prot_incidents.assaulted_with_weapon",
  "child_prot_incidents.assaulted_with_weapon",
  "adult_prot_incidents.forced_work",
  "child_prot_incidents.forced_work",
  "adult_prot_incidents.forcibly_detained",
  "child_prot_incidents.forcibly_detained",
  "adult_prot_incidents.hindered_leave_settlement",
  "child_prot_incidents.hindered_leave_settlement"
  )

less_severe_prot_incidents_vars <-c(
  "adult_prot_incidents.verbally_threatened",
  "child_prot_incidents.verbally_threatened",
  "adult_prot_incidents.assaulted_without_weapon",
  "child_prot_incidents.assaulted_without_weapon",
  "adult_prot_incidents.hindered_leave_district",
  "child_prot_incidents.hindered_leave_district"
  )

data$severe_prot_incidents <- comp_score(data, severe_prot_incidents_vars)
data$less_severe_prot_incidents <- comp_score(data, less_severe_prot_incidents_vars)

# protection concerns

severe_prot_concerns_vars <- c(
  "prot_concerns.violence_maiming",
  "prot_concerns.abduction",
  "prot_concerns.explosive_hazards"
  )

less_severe_prot_concerns_vars <- c(
  "prot_concerns.violence_injuries",
  "prot_concerns.early_marriage",
  "prot_concerns.destruction_property",
  "prot_concerns.theft",
  "prot_concerns.psych_wellbeing"
  )

data$severe_prot_concerns <- comp_score(data, severe_prot_concerns_vars)
data$less_severe_prot_concerns <- comp_score(data, less_severe_prot_concerns_vars)

# explosive hazards

severe_explosive_hazards_vars <- c(
  "explosive_impact.injury_death", 
  "explosive_impact.access_services"
                                   )
less_severe_explosive_hazards_vars <- c(
  "explosive_impact.relocation", 
  "explosive_impact.livelihoods_impact",
  "explosive_impact.psych_impact"
                                        )

data$severe_explosive_hazards <- comp_score(data, severe_explosive_hazards_vars)
data$less_severe_explosive_hazards <- comp_score(data, less_severe_explosive_hazards_vars)


# tazkira

tazkira_total_vars <- c(
  "adult_tazkira", 
  "child_tazkira")
data$tazkira_total <- comp_score(data, tazkira_total_vars)

# Protection Severity Score

## Weights
data <- data %>% 
  mutate(
    prot_incident_class = case_when(
      severe_prot_incidents >= 1 ~ 3,
      severe_prot_incidents == 0 & data$less_severe_prot_incidents >= 1 ~ 2,
      TRUE ~ 0),
    # violence targeting women, girls, boys
    sgbv_incidents_class = case_when(
      other_incidents.sgbv == 1 ~ 2,
      TRUE ~ 0
    ),
    # children working unsafe conditions
    children_work_safety_class = case_when(
      children_work_safety =='yes' ~ 1,
      TRUE ~ 0
    ),
    prot_concerns_class = case_when(
      severe_prot_concerns >= 1 ~ 3,
      severe_prot_concerns == 0 & data$less_severe_prot_concerns >= 1 ~ 2,
      TRUE ~ 0
    ),
    # hh members injured conflict or nat disaster
    injuries_class = case_when(
      adult_injuries_cause %in% c('conflict', 'natural_disaster') |
      child_injuries_cause %in% c('conflict', 'natural_disaster') ~ 3,
      TRUE ~ 0
    ),
    prot_explosive_hazards_class = case_when(
      severe_explosive_hazards >= 1 ~ 3,
      severe_explosive_hazards == 0 & less_severe_explosive_hazards >=1 ~ 2,
      TRUE ~ 0
    ),
    tazkira_class = case_when(
      tazkira_total == 0 ~ 2,
      tazkira_total > 0 & tazkira_total < hh_size ~ 1
      )
    )


# Score

prot_score_vars <- c(
  "prot_incident_class",
  "sgbv_incidents_class",
  "children_work_safety_class",
  "prot_concerns_class",
  "injuries_class",
  "prot_explosive_hazards_class",
  "tazkira_class")

data$prot_score <- comp_score(data, prot_score_vars)

data <- data %>% 
  mutate(
    prot_severity = case_when(
         prot_score <= 2  ~ 1,
         prot_score <= 5  ~ 2,
         prot_score <= 8  ~ 3,
         prot_score <= 18 ~ 4
         ), 
    prot_sev_high = case_when(
      prot_severity >= 3 ~ 1,
      TRUE ~ 0
      )
  )



#################################################################

### ESNFI ####

# shelter type
data$shelter_class<-ifelse(data$shelter == 'open_space',3,ifelse(data$shelter == 'tent' | data$shelter == 'makeshift_shelter' | data$shelter == 'collective_centre' | data$shelter == 'transitional',2,0))

# shelter damage
data$shelter_damage_class<-ifelse(data$shelter_damage_extent== 'fully_destroyed' & data$shelter_damage_repair == 'no',3,
                                  ifelse(data$shelter_damage_extent== 'significant_damage' & data$shelter_damage_repair == 'no',2,
                                         ifelse(data$shelter_damage_extent== 'partial_damage' & data$shelter_damage_repair == 'no',1,0)))

data$shelter_damage_class[is.na(data$shelter_damage_class)] <- 0

# TENENCY AGREEMENT
data$tenancy_class<-ifelse(data$tenancy == 'unofficial',3,ifelse(data$tenancy == 'own_home_without_doc' | data$tenancy == 'rental_verbal' | data$shelter_hosted == 'yes',2,0))

data$tenancy_class[is.na(data$tenancy_class)] <- 0

# blankets
data$blankets_class<-ifelse(data$blankets_number > data$hh_size,3,0)
data$blankets_class[is.na(data$blankets_class)] <- 0

# basic needs
data$sleeping_mats <- car::recode(data$sleeping_mats, " 'yes' = 1; 'no' = 0")
data$tarpaulin <- car::recode(data$tarpaulin, " 'yes' = 1; 'no' = 0")
data$cooking_pots <- car::recode(data$cooking_pots, " 'yes' = 1; 'no' = 0")
data$stainless_steel <- car::recode(data$stainless_steel, " 'yes' = 1; 'no' = 0")
data$water_storage <- car::recode(data$water_storage, " 'yes' = 1; 'no' = 0")
data$hygiene_sanitation <- car::recode(data$hygiene_sanitation, " 'yes' = 1; 'no' = 0")

data$basic_needs_total<-coerc(data[["sleeping_mats"]])+coerc(data[["tarpaulin"]])+coerc(data[["cooking_pots"]])+coerc(data[["stainless_steel"]])+coerc(data[["water_storage"]])+coerc(data[["hygiene_sanitation"]])

data$basic_needs_score<-car::recode(data$basic_needs_total,
                               "0:2=3;
                               3:5=2;
                               6=0")  

# ESNFI Severity Score
data$esnfi_score<-coerc(data[["shelter_class"]])+coerc(data[["shelter_damage_class"]])+coerc(data[["tenancy_class"]])+coerc(data[["blankets_class"]])+coerc(data[["basic_needs_score"]])

data$esnfi_severity<-car::recode(data$esnfi_score,
                            "0:2='1';
                            3:6='2';
                            7:9='3';
                            10:16='4'") 

data$esnfi_sev_high<-ifelse(data$esnfi_severity==3|data$esnfi_severity==4,1,0)

#################################################################

### WASH ####

# water source #
data$water_source_class<-car::recode(data$water_source,
                                "'surface_water'=3;
                                'water_trucking'=2;
                                'spring_unprotected'=2;
                                'spring_protected'=0;
                                'handpump_private'=0;
                                'handpump_public'=0;
                                'piped_public'=0;
                                'other'=0")

# water barriers
data$water_barriers_class<-ifelse(data$water_sufficiency== 'insufficient' &
                                    (data$water_barriers== 'too_far' |
                                       data$water_barriers== 'high_risk' | 
                                       data$water_barriers== 'social_restrictions'),
                                  3,ifelse(data$water_sufficiency== 'insufficient',2,
                                           ifelse(data$water_sufficiency== 'barely_sufficient',1,0)))

data$water_barriers_class[is.na(data$water_barriers)] <- 0

# soap  
data$soap_class<-ifelse(data$soap == 'yes_didnt_see' | data$soap == 'no', 1,0)

# latrines #
data$latrine_class<-ifelse(data$latrine == 'open' | data$latrine == 'public_latrine', 3, 
                           ifelse(data$latrine == 'pit_latrine_uncovered',2,0))

# primary waste dispopsal #
data$waste_disposal_class<-ifelse(data$waste_disposal == 'open_space' | data$waste_disposal == 'burning', 2,0)

#distance to primary water source
data$water_distance_class<-ifelse(data$water_distance == 'over_1km'| data$water_distance == '500m_to_1km',3,0)

# WASH Severity Score
data$wash_score<-coerc(data[["water_source_class"]])+coerc(data[["water_barriers_class"]])+coerc(data[["soap_class"]])+coerc(data[["latrine_class"]])+coerc(data[["waste_disposal_class"]])+coerc(data[["water_distance_class"]])

data$wash_severity<-car::recode(data$wash_score,
                           "0:2='1';
                           3:5='2';
                           6:8='3';
                           9:16='4'")      

data$wash_sev_high<-ifelse(data$wash_severity==3|data$wash_severity==4,1,0)

#################################################################
### Nutrition ####

muac_presence_analysis<-overall_muac_data %>% 
  group_by(`_submission__uuid`) %>% 
  filter(person_muac>=1) %>% 
  summarize(number_muac_person=sum(person_muac),
            number_muac_mod_mal=sum(moderate_malnutrition),
            number_muac_sev_mal=sum(severe_malnutrition),
            min_muac=min(muac_measurement),
            ruft_reception_num = sum(rutf_reception== "yes"),
            ruft_reception = sum(rutf_reception== "yes")>=1)
# Malnutrition present = 1, not present = 0

muac_presence_analysis$malnutrition_present<-ifelse(muac_presence_analysis$number_muac_mod_mal>=1 | muac_presence_analysis$number_muac_sev_mal>=1,1,0) 

# join with parent table  
data<-full_join(data, muac_presence_analysis, by = c("uuid"="_submission__uuid"))


# reported malnourishment (mod & sev muac)
data$muac_score<-ifelse(data$number_muac_sev_mal>1,7,ifelse(data$number_muac_sev_mal==1,6,ifelse(data$number_muac_sev_mal==0 & data$number_muac_mod_mal>1,4,ifelse(data$number_muac_sev_mal==0 & data$number_muac_mod_mal==1,3,0))))

# dietary diversity --- 
# therefore nutrition compotite indicator will exclude hhs with children aged 2-5, since this they are not asked this question ###
data$dietary_div_count<-coerc(data[["minimum_dietary_diversity.staples"]])+coerc(data[["minimum_dietary_diversity.legumes"]])+coerc(data[["minimum_dietary_diversity.dairy"]])+coerc(data[["minimum_dietary_diversity.meat"]])+coerc(data[["minimum_dietary_diversity.eggs"]])+coerc(data[["minimum_dietary_diversity.vitamin_a_veg"]])+coerc(data[["minimum_dietary_diversity.other_veg"]])

data$dietary_div_score<-ifelse(data$dietary_div_count==0,4,ifelse(data$dietary_div_count==1,3,ifelse(data$dietary_div_count==2,2,ifelse(data$dietary_div_count==3,1,0))))
data$dietary_div_score[is.na(data$dietary_div_score)] <- 0

# Nutrition Severity Score
data$nut_score_hh_w_muac<-coerc(data[["muac_score"]])+coerc(data[["dietary_div_score"]])
data$nut_score<-data$nut_score_hh_w_muac
data$nut_score[is.na(data$nut_score)] <- 0

data$nut_severity<-car::recode(data$nut_score,
                          "0:2='1';
                          3:5='2';
                          6:8='3';
                          9:16='4'")  




data$nut_sev_high<-ifelse(data$nut_severity==3|data$nut_severity==4,1,0)

#################################################################

### Education EiE ####

education_analysis<-overall_hh_roster %>% 
  filter(!is.na(current_year_enrolled))

education_analysis$enrolled_and_attending<-ifelse(education_analysis$current_year_enrolled=='no',0,
                                                  ifelse(education_analysis$current_year_enrolled=='yes' & education_analysis$current_year_attending=='no',0,1))

education_analysis$enrolled_1 <- if_else(education_analysis$current_year_enrolled=='no',0,1)
education_analysis$attending_1 <- if_else(education_analysis$current_year_attending=='no',0,1)

education_analysis$total_schoolage_child<-1  

#removal from school due to shock
education_analysis$shock_presence<-coerc(education_analysis[["edu_removal_shock.displacement"]])+coerc(education_analysis[["edu_removal_shock.conflict"]])+coerc(education_analysis[["edu_removal_shock.natural_disaster"]])
education_analysis$shock_presence[is.na(education_analysis$shock_presence)] <- 0

# group dataset into hh
education_analysis_hh<-education_analysis %>% 
  group_by(`_submission__uuid`) %>% 
  summarize(count_school_child=sum(total_schoolage_child),
            count_enrolled_attending=sum(enrolled_and_attending),
            count_current_enrolled = sum(enrolled_1),
            count_current_attending = sum(attending_1),
            count_shock=sum(shock_presence))

# shock weight
education_analysis_hh$shock_class<-ifelse(education_analysis_hh$count_shock >= 1, 5,0)


# percent children enrolled or attending
education_analysis_hh$percent_enrolled= coerc(education_analysis_hh[["count_enrolled_attending"]])/coerc(education_analysis_hh[["count_school_child"]])

education_analysis_hh$enroll_perc_class<-car::recode(education_analysis_hh$percent_enrolled,
                                                "0:0.249=1;
                                                0.25:0.499=2;
                                                0.5:0.749=3;
                                                0.75:1=4")   

# greater than 3 children not attending

education_analysis_hh$count_not_enrolled<-coerc(education_analysis_hh[["count_school_child"]])-coerc(education_analysis_hh[["count_enrolled_attending"]])

education_analysis_hh$count_not_enrolled_class<-ifelse(education_analysis_hh$count_not_enrolled>=3,3,0)


# join with parent table  
data<-full_join(data, education_analysis_hh,by = c("uuid"="_submission__uuid"))

# reasons not attending
data$severe_not_attending<-coerc(data[["boy_unattendance_reason.insecurity"]])+coerc(data[["boy_unattendance_reason.child_works_instead"]])+coerc(data[["girl_unattendance_reason.insecurity"]])+coerc(data[["girl_unattendance_reason.child_works_instead"]])
data$severe_not_attending[is.na(data$severe_not_attending)] <- 0

data$less_severe_not_attending<-coerc(data[["boy_unattendance_reason.lack_facilities"]])+coerc(data[["boy_unattendance_reason.lack_documentation"]])+coerc(data[["boy_unattendance_reason.too_expensive"]])+coerc(data[["girl_unattendance_reason.lack_facilities"]])+coerc(data[["girl_unattendance_reason.lack_documentation"]])+coerc(data[["girl_unattendance_reason.too_expensive"]])
data$less_severe_not_attending[is.na(data$less_severe_not_attending)] <- 0

data$not_attending_class<-ifelse(data$severe_not_attending >= 1,3, ifelse(data$severe_not_attending==0 & data$less_severe_not_attending >=1,2,0))
data$not_attending_class[is.na(data$not_attending_class)] <- 0

# Education Severity Score
data$edu_score_hh_w_schoolage<-coerc(data[["enroll_perc_class"]])+coerc(data[["shock_class"]])+coerc(data[["count_not_enrolled_class"]])+coerc(data[["not_attending_class"]])
data$edu_score<-data$edu_score_hh_w_schoolage
data$edu_score[is.na(data$edu_score)] <- 0

data$edu_severity<-car::recode(data$edu_score,
                          "0:3='1';
                          4:6='2';
                          7:8='3';
                          9:16='4'")   

data$edu_sev_high<-ifelse(data$edu_severity==3|data$edu_severity==4,1,0)


#################################################################
### Health ####

# #deaths under 5 years age
# overall_death_roster$deaths_under5<-ifelse(overall_death_roster$hh_died_age<5,1,0)
# 
# # deaths >= 5
# overall_death_roster$deaths_over5<-ifelse(overall_death_roster$hh_died_age>=5,1,0)  
# 
# 
# # group by hh
# health_analysis<-overall_death_roster %>% 
#   group_by(`_submission__uuid`) %>% 
#   summarize(number_death_under5=sum(deaths_under5),
#             hh_member_died = sum(hh_member_died),
#             number_death_over5=sum(deaths_over5))
# 
# # join with parent dataset
# data<-full_join(data, health_analysis,by = c("_uuid"="_submission__uuid"))
# 
# #deaths under 5 yrs age weight
# data$number_death_under5_class<-ifelse(data$number_death_under5 >= 1, 3,0)
# data$number_death_under5_class[is.na(data$number_death_under5_class)] <- 0
# 
# # deaths >= 5 weight
# data$number_death_over5_class<-ifelse(data$number_death_over5 >= 1, 2,0)
# data$number_death_over5_class[is.na(data$number_death_over5_class)] <- 0

# health facility barriers
data$health_barriers_total<-coerc(data[["health_facility_barriers.unsafe"]])+coerc(data[["health_facility_barriers.cost_services"]])+coerc(data[["health_facility_barriers.cost_medicines"]])+coerc(data[["health_facility_barriers.too_far"]])+coerc(data[["health_facility_barriers.documentation_problems"]])+coerc(data[["health_facility_barriers.insufficient_female_staff"]])+coerc(data[["health_facility_barriers.treatment_refused"]])+coerc(data[["health_facility_barriers.other"]])
data$health_barriers_total[is.na(data$health_barriers_total)] <- 0

data$health_facility_barriers_class<-ifelse(data$health_facility_access == 'no' & data$health_barriers_total>1,3,ifelse(data$health_facility_access == 'no' & data$health_barriers_total==1,2,0))

# health facility distance
data$health_facility_dist_class<-ifelse(data$health_facility_distance == 'none' | data$health_facility_distance=='more_10km',3,ifelse(data$health_facility_distance=='6_10km',2,0))

# health facilities affected
data$health_facility_affected_class<-ifelse(data$health_facility_affected_how == 'forcibly_closed'|data$health_facility_affected_how == 'damaged_conflict'|data$health_facility_affected_how == 'damaged_natural_disasters',3, ifelse(data$health_facility_affected_how=='lack_staff'|data$health_facility_affected_how=='lack_medicine',2,0))
data$health_facility_affected_class[is.na(data$health_facility_affected_class)] <- 0

# health selected as priority need
data$health_priority_need_class<-ifelse(data$priority_needs.healthcare == 1, 3,0)

# behaviour changes as result of conflict
data$behavior_change_cause_class<-ifelse(data$behavior_change_cause == 'yes', 2,0)
data$behavior_change_cause_class[is.na(data$behavior_change_cause_class)] <- 0

# birth location
data$birth_location_class<-ifelse(data$birth_location == 'home'|data$birth_location == 'midwife_home'|data$birth_location == 'outside'|data$birth_location == 'other',1,0)
data$birth_location_class[is.na(data$birth_location_class)] <- 0

# Health Severity Score
data$health_score<- coerc(data[["health_facility_barriers_class"]])+coerc(data[["health_facility_dist_class"]])+coerc(data[["health_facility_affected_class"]])+coerc(data[["health_priority_need_class"]])+coerc(data[["behavior_change_cause_class"]])+coerc(data[["birth_location_class"]])

data$health_severity<-car::recode(data$health_score,
                             "0:2='1';
                             3:5='2';
                             6:8='3';
                             9:16='4'")   

data$health_sev_high<-ifelse(data$health_severity==3|data$health_severity==4,1,0)

#################################################################
# number sectoral needs ####

data$total_sectoral_needs<-coerc(data[["fsac_sev_high"]])+coerc(data[["prot_sev_high"]])+coerc(data[["esnfi_sev_high"]])+coerc(data[["wash_sev_high"]])+coerc(data[["nut_sev_high"]])+coerc(data[["edu_sev_high"]])+coerc(data[["health_sev_high"]])


#################################################################

### LSCI - coping strategies ####

# coping severity
data$lcsi_severity<-car::recode(data$lcsi_category,
                           "'food_secure'='minimal';
                           'marginally_insecure'='stress';
                           'moderately_insecure'='severe';
                           'severely_insecure'='extreme'")   



#################################################################

## Indicators ####


### Numerators
## Some numerators combine variables calcualte those here
data$edu_age_boys_girls_num <-  comp_score(data, c("boys_ed","girls_ed"))

food_water_rent_vars <- c(
  "food_exp",
  "water_expt",
  "rent_exp",
  "total_income"
)
data$food_water_rent_num <- comp_score(data, food_water_rent_vars)



all_expenses_vars <- c(
  "food_exp",
  "water_expt",
  "rent_exp",
  "fuel_exp",
  "debt_exp")
data$all_expenses <- comp_score(data, all_expenses_vars)


min_die_vars <- c(
    "minimum_dietary_diversity.staples",
    "minimum_dietary_diversity.legumes",
    "minimum_dietary_diversity.dairy",
    "minimum_dietary_diversity.meat",
    "minimum_dietary_diversity.eggs",
    "minimum_dietary_diversity.vitamin_a_veg",
    "minimum_dietary_diversity.other_veg")

data$min_die_num <- comp_score(data, min_die_vars)

priority_nfi_vars <- c(
  "sleeping_mats",
  "tarpaulin",
  "cooking_pots",
  "stainless_steel",
  "water_storage",
  "hygiene_sanitation"
)
  
data$priority_nfi_num <- comp_score(data, priority_nfi_vars)

child_vars <- c(
  "males_0_2_total",
  "males_3_5_total",
  "females_0_2_total",
  "females_3_5_total")
data$children_under5 <- comp_score(data, child_vars)

comp_ind_vars <- c(
  "prot_sev_high",
  "fsac_sev_high",
  "esnfi_sev_high",
  "wash_sev_high",
  "nut_sev_high",
  "edu_sev_high",
  "health_sev_high"
)
data$comp_ind_sev <- comp_score(data, comp_ind_vars)

## Age categories

hh_group <- overall_hh_roster %>% 
  mutate(
    age_0_4 =  hh_member_age <=4,
    age_0_14 =  hh_member_age <=14,
    age_10_17 =  hh_member_age >= 10 & hh_member_age <=17,
    age_15_64 =  hh_member_age >= 14 & hh_member_age <=64,
    age_18_64 =  hh_member_age >= 18 & hh_member_age <=64,
    age_65 =  hh_member_age >= 65
  ) %>% 
  group_by(`_submission__uuid`) %>% 
  summarise(
    age_0_4 = sum(age_0_4, na.rm = TRUE),
    age_0_14 = sum(age_0_14, na.rm = TRUE),
    age_10_17 = sum(age_10_17, na.rm = TRUE),
    age_15_64 = sum(age_15_64, na.rm = TRUE),
    age_18_64 = sum(age_18_64, na.rm = TRUE),
    age_65 = sum(age_65, na.rm = TRUE)
  )  

data <- full_join(data, hh_group,by = c("uuid"="_submission__uuid"))

 # Adjust displacement status as more information in other data
non_displ_data <- read.csv("input/Non_Displaced_Host_List_v2.csv",stringsAsFactors=F,na.strings = c("", "NA"))
data<-full_join(data, non_displ_data,by = c("district"="district"))
  data$final_displacement_status_non_displ<-ifelse(data$final_displacement_status=='non_displaced'|data$final_displacement_status=='host', data$non_displ_class,data$final_displacement_status)

# prev_displacement
data <- data %>% 
  mutate(
    # prev_displacement_num
    prev_displacement_num_class = case_when(
      prev_displacement_num == 2 ~ "2",
      prev_displacement_num == 3 ~ "3",
      prev_displacement_num >3 ~ "4+"
    ),
    # refugee_displace_year
    refugee_displace_year_class = case_when(
      refugee_displace_year == 0 ~ "0",
      refugee_displace_year == 1 ~ "1",
      refugee_displace_year == 2 ~ "2",
      refugee_displace_year == 3 ~ "3",
      refugee_displace_year > 3 ~ "4+"
    ),
    # cb_return_displace_year
    cb_return_displace_year_class = case_when(
      cb_return_displace_year == 0 ~ "0",
      cb_return_displace_year == 1 ~ "1",
      cb_return_displace_year == 2 ~ "2",
      cb_return_displace_year == 3 ~ "3",
      cb_return_displace_year > 3 ~ "4+"
    ),
    # cb_return_return_year
    cb_return_return_year_call = case_when(
      cb_return_return_year == 0 ~ "0",
      cb_return_return_year == 1 ~ "1",
      cb_return_return_year == 2 ~ "2",
      cb_return_return_year == 3 ~ "3",
      cb_return_return_year > 3 ~ "4+"
    ),
    # idp_displ_year
    idp_displ_year_class = case_when(
      idp_displ_year == 0 ~ "0",
      idp_displ_year == 1 ~ "1",
      idp_displ_year == 2 ~ "2",
      idp_displ_year == 3 ~ "3",
      idp_displ_year > 3 ~ "4+"
    ),
    # head of household age_group
    hoh_age_group = case_when(
      hoh_age >= 65 ~ "65+",
      hoh_age < 65 ~ "<65"
    ),
    # head of household disabled
    hoh_disabled = case_when(
      wg_walking == "yes" |  wg_selfcare == "yes" ~ "disabled",
      wg_walking == "no" |  wg_selfcare == "no" ~ "not_disabled",
      TRUE ~ NA_character_
    ),
    pregnant_member = case_when(
      pregnant > 0 ~ "at_least_one_mem_pregnant",
      pregnant == 0 ~ "no_mem_pregnent",
      TRUE ~ NA_character_
    ),
    lactating_member = case_when(
      lactating > 0 ~ "at_least_one_mem_lactating",
      lactating == 0 ~ "no_mem_lactating",
      TRUE ~ NA_character_
    ),
    pregnant_lactating_member = case_when(
      pregnant > 0 | lactating > 0 ~ "at_least_one_mem_pregnant_lactating",
      pregnant == 0 & lactating == 0 ~ "no_mem_pregnent_lactating",
      TRUE ~ NA_character_
    ),
    female_literacy_yes_no <- case_when(
      female_literacy == 0 ~ "0",
      female_literacy >= 1 ~ "1 or more",
      TRUE ~ NA_character_
    ),
    male_literacy_yes_no <- case_when(
      male_literacy == 0 ~ "0",
      male_literacy >= 1 ~ "1 or more",
      TRUE ~ NA_character_
    ),
    # How many adults 18+ years worked outside of the household in the last 30 days?
    adults_working_yes_no = case_when(
      adults_working == 0 ~ "0",
      adults_working >= 1 ~ "1 or more",
      TRUE ~ NA_character_
    ),
    children_working_yes_no = case_when(
      children_working == 0 ~ "0",
      children_working >= 1 ~ "1 or more",
      TRUE ~ NA_character_
    ),
    ag_income_cal = case_when(
      ag_income == 0 ~ 0,
      ag_income > 0 ~ ag_income / hh_size,
      TRUE ~ NA_real_
    ),
    livestock_income_cal = case_when(
      livestock_income == 0 ~ 0,
      livestock_income > 0 ~ livestock_income / hh_size,
      TRUE ~ NA_real_
    ),
    rent_income_cal = case_when(
      rent_income == 0 ~ 0,
      rent_income > 0 ~ rent_income / hh_size,
      TRUE ~ NA_real_
    ),
    small_business_income_cal = case_when(
      small_business_income == 0 ~ 0,
      small_business_income > 0 ~ small_business_income / hh_size,
      TRUE ~ NA_real_
    ),
    unskill_labor_income_cal = case_when(
      unskill_labor_income == 0 ~ 0,
      unskill_labor_income > 0 ~    unskill_labor_income / hh_size,
      TRUE ~ NA_real_
    ),
    skill_labor_income_cal = case_when(
      skill_labor_income == 0 ~ 0,
      skill_labor_income > 0 ~    skill_labor_income / hh_size,
      TRUE ~ NA_real_
    ),
    formal_employment_income_cal = case_when(
      formal_employment_income == 0 ~ 0,
      formal_employment_income > 0 ~    formal_employment_income / hh_size,
      TRUE ~ NA_real_
    ),
    gov_benefits_income_cal = case_when(
      gov_benefits_income == 0 ~ 0,
      gov_benefits_income > 0 ~ gov_benefits_income / hh_size,
      TRUE ~ NA_real_
    ),
    hum_assistance_income_cal = case_when(
      hum_assistance_income == 0 ~ 0,
      hum_assistance_income > 0 ~ hum_assistance_income / hh_size,
      TRUE ~ NA_real_
    ),
    remittance_income_cal = case_when(
      remittance_income == 0 ~ 0,
      remittance_income > 0 ~ remittance_income / hh_size,
      TRUE ~ NA_real_
    ),
    loans_income_cal = case_when(
      loans_income == 0 ~ 0,
      loans_income > 0 ~    loans_income / hh_size,
      TRUE ~ NA_real_
    ),
    asset_selling_income_cal = case_when(
      asset_selling_income == 0 ~ 0,
      asset_selling_income > 0 ~    asset_selling_income / hh_size,
      TRUE ~ NA_real_
    ),
    total_income_cal = case_when(
      total_income == 0 ~ 0,
      total_income > 0 ~    total_income / hh_size,
      TRUE ~ NA_real_
    ),
    # Debt level
    debt_amount_cal = case_when(
      debt_amount == 0 ~ 0,
      debt_amount > 0 ~    debt_amount / hh_size,
      TRUE ~ NA_real_),
    food_exp_cal = case_when(
      total_income == 0 ~ 0,
      total_income > 0 ~ food_exp / total_income,
      TRUE ~ NA_real_
    ),
    water_expt_cal = case_when(
      total_income == 0 ~ 0,
      total_income > 0 ~ water_expt / total_income,
      TRUE ~ NA_real_
    ),
    rent_exp_cal = case_when(
      total_income == 0 ~ 0,
      total_income > 0 ~ rent_exp / total_income,
      TRUE ~ NA_real_
    ),
    fuel_exp_cal = case_when(
      total_income == 0 ~ 0,
      total_income > 0 ~ fuel_exp / total_income,
      TRUE ~ NA_real_
    ),
    debt_exp_cal = case_when(
      total_income == 0 ~ 0,
      total_income > 0 ~ debt_exp / total_income,
      TRUE ~ NA_real_
    ),
    basic_needs_cal = case_when(
      food_water_rent_num == 0 ~ 0,
      food_water_rent_num > 0 ~ food_water_rent_num / total_income,
      TRUE ~ NA_real_
    ),
    minimum_dietary_diversity_cal = case_when(
      min_die_num >= 4 ~ "4 food groups",
      min_die_num < 4 ~  "<4 food groups",
      TRUE ~ NA_character_
    ),
    rooms_hh_cal = case_when(
      rooms == 0 ~ 0,
      rooms > 0 ~    rooms / hh_size,
      TRUE ~ NA_real_
    ),
    blankets_people_cal = case_when(
      blankets_number == 0 ~ 0,
      blankets_number > 0 ~    blankets_number / hh_size,
      TRUE ~ NA_real_
    ),
    blankets_suff_cal = case_when(
      blankets_people_cal < 1 ~ "<1",
      blankets_people_cal >= 1 ~ "1+",
      TRUE ~ NA_character_
    ),
    priority_nfi_cal = case_when(
      priority_nfi_num <= 1 ~ "0-1",
      priority_nfi_num <= 3 ~ "2-3",
      priority_nfi_num <= 5 ~ "4-5",
      priority_nfi_num <= 6 ~ "6",
      TRUE ~ NA_character_
    ),
    imp_energy_source1_cal = case_when(
      energy_source %in% c("wood" , "animal_waste" , "paper_waste") ~ 1,
      TRUE ~ 0
    ),  
    imp_energy_source2_cal = case_when(
      energy_source %in% c("coal" , "charcoal" , "lpg" , "electricity") ~ 1,
      TRUE ~ 0
    ),  
    diarrhea_cases_cal = case_when(
      diarrhea_cases == 0 ~ 0,
      diarrhea_cases > 0 ~    diarrhea_cases / children_under5,
      TRUE ~ NA_real_
    ), 
    imp_water_source1_cal = case_when(
      water_source %in% c("handpump_private", "handpump_public",
                          "piped_public", "spring_protected") ~ 1,
      TRUE ~ 0
    ),  
    imp_water_source2_cal = case_when(
      water_source %in% c("spring_unprotected","surface_water"
                          , "water_trucking", "other") ~ 1,
      TRUE ~ 0
    ),  
    imp_san_source1_cal = case_when(
      water_source %in% c("open", "pit_latrine_uncovered", 
                          "other") ~ 1,
      TRUE ~ 0
    ),  
    imp_san_source2_cal = case_when(
      water_source %in% c("public_latrine", "pit_latrine_covered",
                          "vip_latrine", "flush_toilet_open_drain",
                          "flush_toilet_septic") ~ 1,
      TRUE ~ 0
    ),  
    comp_ind_sev_2_call = case_when(
      comp_ind_sev >= 2 ~ ">=2",
      comp_ind_sev <2 ~ "<2",
      TRUE ~ NA_character_
    ),
    dep_ratio_call =  case_when(
      age_0_14 == 0 & age_65 == 0 ~ 0,
      (age_0_14 > 0 | age_65 > 0) ~ 
        sum(age_0_14,age_65, na.rm = TRUE)/sum(age_15_64,na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    female_lit_call =  case_when(
      female_literacy == 0 ~ 0,
      female_literacy == 0 ~ 
        female_literacy/sum(females_11_17_total,females_18_plus_total, na.rm=TRUE),
      TRUE ~ NA_real_
    ),
    male_lit_call =  case_when(
      male_literacy == 0 ~ 0,
      male_literacy == 0 ~ 
        male_literacy/sum(males_11_17_total,males_18_plus_total, na.rm=TRUE),
      TRUE ~ NA_real_
    ),
    adult_behavior_change_call =  case_when(
      adult_behavior_change == "yes" ~ 1,
      adult_behavior_change == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    child_behavior_change_call =  case_when(
      child_behavior_change == "yes" ~ 1,
      child_behavior_change == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    atleast_one_behav_change_call = case_when(
      child_behavior_change_call == 0 & adult_behavior_change_call == 0 ~ 0,
      child_behavior_change_call > 0 & adult_behavior_change_call > 0 ~ 1,
      TRUE ~ NA_real_
    ),
    adults_working_call = case_when(
      adults_working == 0 ~ 0,
      adults_working > 0 ~ adults_working/age_18_64,
      TRUE ~ NA_real_
    ),
    child_working_call = case_when(
      children_working == 0 ~ 0,
      children_working > 0 ~ children_working/age_10_17,
      TRUE ~ NA_real_
    ),
    adult_tazkira_cal = case_when(
      adult_tazkira == 0 ~ "0",
      adult_tazkira >= 1 ~ ">=1",
      TRUE ~ NA_character_
    ),
    child_tazkira_cal = case_when(
      child_tazkira == 0 ~ "0",
      child_tazkira >= 1 ~ ">=1",
      TRUE ~ NA_character_
    ),
    child_tazkira_cal = case_when(
      child_tazkira == 0 ~ "0",
      child_tazkira >= 1 ~ ">=1",
      TRUE ~ NA_character_
    ),
    any_tazkira_cal = case_when(
      adult_tazkira == 0 & child_tazkira == 0~ "0",
      adult_tazkira >= 1 | child_tazkira >= 1~ ">=1",
      TRUE ~ NA_character_
    ),
    child_working_call = case_when(
      children_working == 0 ~ 0,
      children_working > 0 ~ children_working/age_10_17,
      TRUE ~ NA_real_
    ),
    count_current_enrolled_avg = count_current_enrolled / edu_age_boys_girls_num,
    count_current_attending_avg = count_current_attending / edu_age_boys_girls_num
)




#Recoding new variables
data$hh_no_tazkira <- ifelse(data$tazkira_total < 1, "Tazkira_No", "Tazkira_Yes")

data$muac_yes_no <- ifelse(data$muac_total > 0 & !is.na(data$min_muac)  ,"Yes","No")
data$recent_non_recent <- ifelse(data$final_displacement_status_non_displ == "recent_idps", "recent_idps",
                                 ifelse(data$final_displacement_status_non_displ == "non_recent_idps", "non_recent_idps", NA ))
data$edu_removal_shock_cal <-  ifelse(data$shock_class == 5, "Yes", "No") 
data$enrolled_attending <- ifelse(data$count_enrolled_attending > 0, "Enrolled_and_Attending", "Not" ) 









 

source("r/prepare_disagg.R")

write.csv(data, "./input/data/recoded/data_with_strata.csv", row.names = F)
