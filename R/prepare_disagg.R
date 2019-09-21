# Title: Preparation of data for woa survey
# Authors: Sayed Nabizada, Christopher Jarvis, 
# Date created: 20/09/2019
# Date last changed: 20/09/2019
# Purpose: This script is for recoding the disagg variables in the whole of 
#          of Afghanistan survey data
#          indicators and composite scores are created. 



## Disaggregation variables #####

data <- data %>%
  mutate(
    # Population group
    
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
      wg_walking == "no" |  wg_selfcare == "no" ~ "not_disabled",
      TRUE ~ NA_character_
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
      female_literacy == 0 & male_literacy == 0 ~ "no",
      female_literacy == 1 | male_literacy == 1 ~ "yes",
      TRUE ~ NA_character_
    ),
    # Debt level
    hoh_debt_disagg = case_when(
      debt_amount >= 200000 ~ "high_debt",
      debt_amount >= 50000 & debt_amount < 200000 ~ "medium_debt",
      debt_amount > 0 & debt_amount < 50000 ~ "low_debt",
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
    # Registration of returnees
    registered_dissagg = case_when(
      cb_return_documentation %in% c("yes_shown", "yes") ~ "registered",
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
      final_displacement_status == "host" ~ "host",
      final_displacement_status %in% c("recent_idps", "non_recent_idp", "returnee" ~ "displaced",
                                       TRUE ~ NA_character_
      ),
      disp_length_disagg = case_when(
        final_displacement_status == "recent_idps" ~ "recent",
        idp_displ_year <= 1 ~ "prolonged",
        idp_displ_year >= 2 ~ "protracted",
        TRUE ~ NA_character_
      ),
      
      
    )
    