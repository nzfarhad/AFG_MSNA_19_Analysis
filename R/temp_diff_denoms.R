
d <- overall_hh_roster

# create the variables
overall_hh_roster <- overall_hh_roster %>% 
  mutate(
    school_age = case_when(
      hh_member_age >=6 & hh_member_age <= 18 ~ "school age",
      TRUE ~ "not school age"
    ),
    male_0_5 = male_0_2 + male_3_5,
    male_6_18 = male_6_12 + male_13_18,
    female_0_5 = female_0_2 + female_3_5,
    female_6_18 = female_6_12 + female_13_18,
  )

tot_pop <- data.frame(row.names = 1)
d <- overall_hh_roster
tot_pop$tot_child_6_18 <- sum(d$school_age == "school age") 
tot_pop$perc_females <- sum(d$hh_member_sex=="female",na.rm = TRUE) /nrow(d)
tot_pop$perc_males <- sum(d$hh_member_sex=="male",na.rm = TRUE)/nrow(d)
filt <- d$hh_member_sex=="male"
tot_pop$perc_male_0_5 <- sum(d$male_0_5,na.rm = TRUE)/nrow(d[filt,])
tot_pop$perc_male_6_18 <- sum(d$male_6_18, na.rm = TRUE)/nrow(d[filt,])
tot_pop$perc_male_19_59 <- sum(d$male_19_59, na.rm = TRUE)/nrow(d[filt,])
tot_pop$perc_male_60 <- sum(d$male_60_plus, na.rm = TRUE)/nrow(d[filt,])
filt <- d$hh_member_sex=="female"
tot_pop$perc_female_0_5 <- sum(d$female_0_5,na.rm = TRUE)/nrow(d[filt,])
tot_pop$perc_female_6_18 <- sum(d$female_6_18, na.rm = TRUE)/nrow(d[filt,])
tot_pop$perc_female_19_59 <- sum(d$female_19_59, na.rm = TRUE)/nrow(d[filt,])
tot_pop$perc_female_60 <- sum(d$female_60_plus, na.rm = TRUE)/nrow(d[filt,])

filt <- d$male_6_12>0
tot_pop$perc_enrolled_male_6_12 <- sum(d[filt]$current_year_enrolled, na.rm = TRUE)/nrow(d[filt,])
tot_pop$perc_attending_male_6_12 <- sum(d[filt]$current_year_attending, na.rm = TRUE)/nrow(d[filt,])
filt <- d$male_13_18>0
tot_pop$perc_enrolled_male_13_18 <- sum(d[filt]$current_year_enrolled, na.rm = TRUE)/nrow(d[filt,])
tot_pop$perc_attending_male_13_18 <- sum(d[filt]$current_year_attending, na.rm = TRUE)/nrow(d[filt,])
filt <- d$female_6_12>0
tot_pop$perc_attending_female_6_12 <- sum(d[filt]$current_year_attending, na.rm = TRUE)/nrow(d[filt,])
filt <- d$female_13_18>0
tot_pop$perc_attending_female_13_18 <- sum(d[filt]$current_year_attending, na.rm = TRUE)/nrow(d[filt,])

filt <- d$school_age == "school age"
tot_pop$perc_enrolled_previous <- sum(d[filt]$previous_year_enrolled, na.rm = TRUE)/nrow(d[filt,])
tot_pop$perc_edu_removal <- sum(d[filt]$edu_removal_shock.no, na.rm = TRUE)/nrow(d[filt,])


denom_ <- sum(data$age_0_4,na.rm = TRUE)
tot_pop$perc_moderate <- sum(data$number_muac_mod_mal, na.rm = TRUE)/denom_
tot_pop$perc_severe <- sum(data$number_muac_sev_mal, na.rm = TRUE)/denom_
tot_pop$perc_muac125 <- sum(data$min_muac>125, na.rm = TRUE)/denom_
tot_pop$perc_ruft_recpt <- sum(data$ruft_reception, na.rm = TRUE)/denom_

filt <- data$min_muac < 115
denom_ <- sum(data[filt,]$age_0_4,na.rm = TRUE)
tot_pop$perc_ruft_recpt_115 <- sum(data[filt,]$ruft_reception, na.rm = TRUE)/denom_

filt <- data$min_muac<= 115 & data$min_muac < 125
denom_ <- sum(data[filt,]$age_0_4,na.rm = TRUE)
tot_pop$perc_ruft_recpt_115_125 <- sum(data[filt,]$ruft_reception, na.rm = TRUE)/denom_

filt <- data$min_muac >= 125
denom_ <- sum(data[filt,]$age_0_4,na.rm = TRUE)
tot_pop$perc_ruft_recpt_125 <- sum(data[filt,]$ruft_reception, na.rm = TRUE)/denom_

fun1 <- function(x){
  round(x*100,0)
}

# Change the format
tot_pop <-  tot_pop %>% 
  mutate_at(vars(contains("perc")), fun1)





