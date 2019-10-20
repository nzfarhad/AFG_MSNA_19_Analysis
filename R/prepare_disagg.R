# Title: Preparation of data for woa survey
# Authors: Sayed Nabizada, Christopher Jarvis, 
# Date created: 21/09/2019
# Date last changed: 21/09/2019
# Purpose: This script is for recoding the disagg variables in the whole of 
#          of Afghanistan survey data
#          indicators and composite scores are created elsewhere. 



## Disaggregation variables #####

data <- data %>%
  mutate(
    # Population group - still needs to be determined. 
    # Region
    region_disagg = region,
    # Province
    province_disagg = province,
    # Head of household sex
    hoh_sex_disagg = hoh_sex,
    # Head of household age
    hoh_elderly_disagg = case_when(
      hoh_age >= 65 ~ "elderly",
      hoh_age < 65 ~ "non-elderly",
      TRUE ~ NA_character_
    ),
    # Head of household disability
    hoh_disabled_disagg = case_when(
      wg_walking == "yes" |  wg_selfcare == "yes" ~ "disabled",
      # wg_walking == "no" &  wg_selfcare == "no" ~ "not_disabled",
      # TRUE ~ NA_character_
      TRUE ~ "not_disabled"
    ),
    # Urban or Rural
    urban_disagg = urban,
    # Head of household chronic illness
    hoh_chronic_illness_disagg = chronic_illness,
    # Previously displaced
    displacements_disagg  = case_when(
      prev_displacement == "no" ~ "no",
      prev_displacement_num >= 2 ~ ">= 2",
      TRUE ~ NA_character_
    ),
    # Literate adults
    literate_adult_disagg = case_when(
      female_literacy == 0 & male_literacy == 0 ~ "no_literate_adults",
      female_literacy >= 1 | male_literacy >= 1 ~ "literate_adults",
      TRUE ~ NA_character_
    ),
    # Debt level
    hoh_debt_disagg = case_when(
      debt_amount >= 200000 ~ "high_debt",
      debt_amount >= 50000 & debt_amount < 200000 ~ "medium_debt",
      debt_amount > 0 & debt_amount < 50000 ~ "low_debt",
      debt_amount == 0 ~ "no_debt",
      debt == "no" ~ "no_debt",
      TRUE ~ NA_character_
    ),
    # Tazkira # removed some_have as per gss 20190921
    tazkira_disagg = case_when(
      child_tazkira == 0 & adult_tazkira == 0 ~ "non_have_tazkira",
      # adult_lack_tazkira >= 1 | child_lack_tazkira >= 1 ~ "some_have_tazkira",
      adult_lack_tazkira == 0 & child_lack_tazkira == 0 ~ "all_have_tazkira",
      TRUE ~ NA_character_
    ),
    # Tazkira # added some_have as per gss 25190921
    tazkira_disagg2 = case_when(
      child_tazkira == 0 & adult_tazkira == 0 ~ "non_have_tazkira",
      adult_lack_tazkira == 0 | child_lack_tazkira == 0 ~ "some_have_tazkira",
      adult_lack_tazkira == 0 & child_lack_tazkira == 0 ~ "all_have_tazkira",
      TRUE ~ NA_character_
    ),
    # Tazkira # some have tazkira vs non-have tazkira as per gss 29190921
    tazkira_disagg3 = case_when(
      child_tazkira == 0 & adult_tazkira == 0 ~ "non_have_tazkira",
      child_tazkira >= 1 | adult_tazkira >= 1 ~ "some_have_tazkira",
      TRUE ~ NA_character_
    ),
    
    # Registration of returnees
    registered_dissagg = case_when(
      cb_return_documentation == "yes_shown" | cb_return_documentation == "yes_not_shown" ~ "registered",
      cb_return_documentation == "no" ~ "not_registered",
      TRUE ~ NA_character_
    ),
    # LCSI score
    lcsi_disagg = case_when(
      lcsi_score >= 3 ~ "high_lsci",
      lcsi_score <= 2 ~ "low_lsci",
      TRUE ~ NA_character_
    ),
    # Host vs. Displaced (IDPs and returnees)
    host_disagg = case_when(
      final_displacement_status_non_displ == "host" ~ "host",
      final_displacement_status_non_displ %in% c("recent_idps", "non_recent_idps", "cross_border_returnees") ~ "displaced",
      TRUE ~ NA_character_
    ),
    # IDP Displacement Length
    disp_length_disagg = case_when(
      final_displacement_status_non_displ == "recent_idps" ~ "recent",
      idp_displ_year <= 1 ~ "prolonged",
      idp_displ_year >= 2 ~ "protracted",
      TRUE ~ NA_character_
    ),
    # HoH sex version 2
    hoh_sex2_disagg = case_when(
      hoh_sex =="male" | hoh_marital_status == "married" ~ "male",
      hoh_sex == "female" & hoh_marital_status %in% c("single", "widowed", "divorced", "married_elsewhere_afg", "married_elsewhere_outside","no_answer") ~ "female",
      TRUE ~ NA_character_
    ),
    # Behavioural change
    behav_change_disagg = case_when(
      adult_behavior_change == "no" & is.na(child_behavior_change) ~ "no",
      adult_behavior_change == "no_answer" & child_behavior_change == "no_answer" ~ "no",
      adult_behavior_change == "no_answer" & is.na(child_behavior_change) ~ "no",
      adult_behavior_change == "yes" | child_behavior_change == "yes" ~ "yes",
      adult_behavior_change == "no" &  child_behavior_change == "no" ~ "no",
      TRUE ~ NA_character_
    ),
    # Acute Watery Diarrhea (AWD)
    awd_disagg = case_when(
      diarrhea_cases >= 1 ~ "awd_present",
      diarrhea_cases == 0 ~ "no_awd",
      # adult_behavior_change == "no" & child_behavior_change == 0 ~ "some behavioural change",
      TRUE ~ NA_character_
    ),
    #Water source 
    water_src_disagg = case_when(
      water_source == "handpump_private" | water_source == "handpump_public" |
        water_source == "piped_public" | water_source == "spring_protected" ~ "improved",
      water_source == "spring_unprotected" | water_source == "surface_water" |
        water_source == "water_trucking" | water_source == "other" ~ "unimproved",
      TRUE ~ NA_character_
    ),
    #Soap
    soap_disagg = case_when(
      soap == "yes_saw" | soap == "yes_didnt_see" ~ "yes",
      soap == "no"  ~ "no",
      TRUE ~ NA_character_
    ),
    # Child behavior change
    child_behav_change_disagg = case_when(
      child_behavior_change == "yes" ~ "yes",
      child_behavior_change == "no" ~ "no",
      child_behavior_change == "no_answer" ~ "no",
      TRUE ~ NA_character_
    )
)