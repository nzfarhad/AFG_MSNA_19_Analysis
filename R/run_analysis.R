
library(tidyverse)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(hypegrammaR) # stats 4 complex samples
library(composr) # horziontal operations
library(lubridate)
library(parallel)
library(readxl)

source("R/functions/to_alphanumeric_lowercase.R") # function to standardise column headers (like check.names)
source("R/functions/analysisplan_factory.R")  # generate analysis plans
source("R/functions/remove_responses_from_sumstat.R")  # generate analysis plans


# load questionnaire inputs
questions <- read.csv("input/questionnaire/questionnaire_questions.csv", 
                      stringsAsFactors=F, check.names = F)
choices <- read.csv("input/questionnaire/questionnaire_choices.csv", 
                    stringsAsFactors=F, check.names = F)


#######  sampling frame entire population
# samplingframe <- load_samplingframe("input/sampling_frames/sampling_frame_pop_group.csv")

# Displaced only
# samplingframe <- load_samplingframe("input/sampling_frames/sampling_frame_overall.csv")

# Shock affected
samplingframe <- load_samplingframe("input/sampling_frames/sampling_frame_displaced_non_displaced.csv")

# none-displaced
# samplingframe <- load_samplingframe("input/sampling_frames/sampling_frame_non_displaced_only.csv")



# load analysis plan
analysisplan <- load_analysisplan(file = "./input/analysisplans/final_analysis/analysisplan_msni_drivers_all_population.csv")


# Load recoded clean data
response <- read.csv("./input/data/recoded/data_with_strata2.csv") %>%
  mutate(strata = stringr::str_c(final_displacement_status_non_displ, "_", province),
         cluster_id = str_c(final_displacement_status_non_displ, "_", province, "_", survey_village)) %>%
  filter(strata %in% samplingframe$strata)

# check inputs
# kobostandards::check_input(data = response, questions = questions, choices = choices ,samplingframe = samplingframe,
#                            analysisplan = analysisplan) %>% write.csv("./output/check_input.csv")

# Further data cleaning
names(response)<-to_alphanumeric_lowercase(names(response))

response <- response %>%
  mutate(prev_displacement_num = ifelse(is.na(prev_displacement_num), 0, prev_displacement_num)) %>%
  select(final_displacement_status, strata, cluster_id, region:cluster_id)


##### Load questinonnaire
questionnaire <- load_questionnaire(data = response,
                                    questions = questions,
                                    choices = choices)




# wieghts
strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe,
                 sampling.frame.population.column = "population",
                 sampling.frame.stratum.column = "strata",
                 data.stratum.column = "strata",
                 data = response)



response$general_weights <- strata_weight_fun(response)


results <- from_analysisplan_map_to_output(response, analysisplan = analysisplan,
                                          weighting = strata_weight_fun,
                                          cluster_variable_name = "cluster_id",
                                          questionnaire)


### results output
labeled_results <- lapply(results$results, map_to_labeled,questionnaire)
map_to_master_table(results_object =labeled_results, filename = "./output/final_results/results_analysisplan_msni_drivers_all_population.csv")


# pop_group_pivot <- response %>% group_by(final_displacement_status_non_displ) %>% tally()



big_table <- results$results %>% lapply(function(x) x[["summary.statistic"]]) %>% do.call(rbind, .)
write.csv(big_table,"./output/results_prot_new_indicator_short_names.csv")



# not sure if this function should be "user facing" or have some wrappers (@Bouke thoughts?)
# essentially it handles all the looping over different column values as hierarchies.
# then each result is visualised by a function passed here that decides how to render each individual result
# see ?hypegrammaR:::map_to_generic_hierarchical_html

# hypegrammaR:::map_to_generic_hierarchical_html(results,
#                                                render_result_with = hypegrammaR:::from_result_map_to_md_table,
#                                                by_analysisplan_columns = c("dependent.var","repeat.var.value"),
#                                                by_prefix =  c("",""),
#                                                level = 2,
#                                                questionnaire = questionnaire,
#                                                label_varnames = TRUE,
#                                                dir = "./output",
#                                                filename = "rresults_prot_new_indicator_4_vulnerability_disagg.html")
# 
# 
# browseURL("./output/rresults_prot_new_indicator_4_vulnerability_disagg.html")





