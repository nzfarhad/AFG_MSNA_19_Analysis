# Title: Preparation of data for woa survey
# Authors: Sayed Nabizada, Christopher Jarvis, 
# Date created: 20/09/2019
# Date last changed: 20/09/2019
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
data <- read_excel(master_data, sheet = "MSNA_AFG_19_parent_sheet", na = c("","NA"))

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
      ),
    )

fsac_vars <- c("fcs_category_class", "hhs_category_class", "food_source_class", "ag_impact_class", "ls_impact_class")
data$fsac_score <- comp_score(data, fsac_vars)

data <- data %>% 
  mutate(
    fsac_severity = case_when(
      fsac_score <= 1 ~ 1, 
      fsac_score <= 3 ~ 2,
      fsac_score <= 7 ~ 3,
      fsac_score <= 16 ~ 4,
    ),
    fsac_sev_high = case_when(
      fsac_severity <= 2 ~ 0,
      fsac_severity <= 4 ~ 1
      )
    )


summ(mtcars)


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
      tazkira_total > 0 & tazkira_total < hh_size ~ 1,
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
data$sleeping_mats <- recode(data$sleeping_mats, " 'yes' = 1; 'no' = 0")
data$tarpaulin <- recode(data$tarpaulin, " 'yes' = 1; 'no' = 0")
data$cooking_pots <- recode(data$cooking_pots, " 'yes' = 1; 'no' = 0")
data$stainless_steel <- recode(data$stainless_steel, " 'yes' = 1; 'no' = 0")
data$water_storage <- recode(data$water_storage, " 'yes' = 1; 'no' = 0")
data$hygiene_sanitation <- recode(data$hygiene_sanitation, " 'yes' = 1; 'no' = 0")

data$basic_needs_total<-coerc(data[["sleeping_mats"]])+coerc(data[["tarpaulin"]])+coerc(data[["cooking_pots"]])+coerc(data[["stainless_steel"]])+coerc(data[["water_storage"]])+coerc(data[["hygiene_sanitation"]])

data$basic_needs_score<-recode(data$basic_needs_total,
                               "0:2=3;
                               3:5=2;
                               6=0")  

# ESNFI Severity Score
data$esnfi_score<-coerc(data[["shelter_class"]])+coerc(data[["shelter_damage_class"]])+coerc(data[["tenancy_class"]])+coerc(data[["blankets_class"]])+coerc(data[["basic_needs_score"]])

data$esnfi_severity<-recode(data$esnfi_score,
                            "0:2='1';
                            3:6='2';
                            7:9='3';
                            10:16='4'") 

data$esnfi_sev_high<-ifelse(data$esnfi_severity==3|data$esnfi_severity==4,1,0)

#################################################################

### WASH ####

# water source #
data$water_source_class<-recode(data$water_source,
                                "'surface_water'=3;
                                'water_trucking'=2;
                                'spring_unprotected'=2;
                                'spring_protected'=0;
                                'handpump_private'=0;
                                'handpump_public'=0;
                                'piped_public'=0;
                                'other'=0")

# water barriers
data$water_barriers_class[is.na(data$water_barriers)] <- 0
data$water_barriers_class<-ifelse(data$water_sufficiency== 'insufficient' & (data$water_barriers== 'too_far' | data$water_barriers== 'high_risk' | data$water_barriers== 'social_restrictions'),3,ifelse(data$water_sufficiency== 'insufficient',2,
                                                                                                                                                                                                         ifelse(data$water_sufficiency== 'barely_sufficient',1,0)))

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

data$wash_severity<-recode(data$wash_score,
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
            min_muac=min(muac_measurement))

# Malnutrition present = 1, not present = 0

muac_presence_analysis$malnutrition_present<-ifelse(muac_presence_analysis$number_muac_mod_mal>=1 | muac_presence_analysis$number_muac_sev_mal>=1,1,0) 

# join with parent table  
data<-full_join(data, muac_presence_analysis,by = c("uuid"="_submission__uuid"))


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

data$nut_severity<-recode(data$nut_score,
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

education_analysis_hh$enroll_perc_class<-recode(education_analysis_hh$percent_enrolled,
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

data$edu_severity<-recode(data$edu_score,
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
# data<-full_join(data, health_analysis,by = c("uuid"="_submission__uuid"))
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

data$health_severity<-recode(data$health_score,
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
data$lcsi_score

data$lcsi_severity<-recode(data$lcsi_category,
                           "'food_secure'='minimal';
                           'marginally_insecure'='stress';
                           'moderately_insecure'='severe';
                           'severely_insecure'='extreme'")   

#################################################################

## Grouping indicators ####



## Disaggregation variables #####
## Need to group these into place


# Adjust displacement status
non_displ_data <- read.csv("input/Non_Displaced_List.csv",stringsAsFactors=F,na.strings = c("", "NA"))
data<-full_join(data, non_displ_data,by = c("district"="district"))
data$final_displacement_status_non_displ<-ifelse(data$final_displacement_status=='non_displaced'|data$final_displacement_status=='host', data$non_displ_class,data$final_displacement_status)


#Recoding new variables
data$hoh_elderly <- ifelse(data$hoh_age >= 65, "elderly", "non-elderly")
data$hoh_age_group <- ifelse(data$hoh_age >= 60, "60+", 
                             ifelse(data$hoh_age <= 60 & data$hoh_age > 17,"18-59",
                                    ifelse(data$hoh_age <= 17 & data$hoh_age > 6, "6-17","0-5")))

data$hh_no_tazkira <- ifelse(data$tazkira_total < 1, "Tazkira_No", "Tazkira_Yes")

data$muac_yes_no <- ifelse(data$muac_total > 0 & !is.na(data$min_muac)  ,"Yes","No")

data <- data %>%
  mutate(hoh_disabled = ifelse(wg_walking == "yes" |  wg_selfcare == "yes", "disable", "not_disable"))

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
  mutate(tazkira_disagg = case_when(
    child_tazkira == 0 & adult_tazkira == 0 ~ "non_have_tazkira",
    adult_lack_tazkira >= 1 | child_lack_tazkira >= 1 ~ "some_have_tazkira",
    adult_lack_tazkira == 0 & child_lack_tazkira == 0 ~ "all_have_tazkira"
  ),
  
  debt_disagg = case_when(
    debt_amount >= 200000 ~ "high_debt",
    debt_amount >= 50000 & debt_amount < 200000 ~ "medium_debt",
    debt_amount > 0 & debt_amount < 50000 ~ "low_debt",
    debt == "no" ~ "no_debt"
  ),
  
  disp_disagg = case_when(
    final_displacement_status_non_displ == "recent_idps" ~ "recent",
    idp_displ_year == 0 | idp_displ_year == 1 ~ "prolonged",
    idp_displ_year >= 2 ~ "protracted"
  ),
  
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
