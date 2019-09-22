dmerge <- function (results, rows = c("repeat.var", "repeat.var.value"), 
                    values = "numbers", ignore = c("se", "min", 
                                                   "max"), questionnaire = NULL) 
{
  all_summary_statistics <- results
  # all_summary_statistics <- all_summary_statistics %>% select(dependent.var, dependent.var.value, independent.var,
  #                                                             independent.var.value, repeat.var, repeat.var.value, numbers )
  
  all_summary_statistics <- results %>% lapply(function(x) {
    x$summary.statistic %>% lapply(function(x) {
      if (is.factor(x)) {
        return(as.character(x))
      }
      x
    }) %>% as.data.frame(stringsAsFactors = F)
  }) %>% do.call(rbind, .)
  

  # deal with Na Disaggs
  all_summary_statistics <- all_summary_statistics %>% 
    filter(!is.na(numbers))
  
  #round %
  all_summary_statistics <- all_summary_statistics %>% 
    mutate(numbers = case_when(
      is.na(independent.var.value) ~ numbers,
      !is.na(independent.var.value) ~ round(numbers * 100, 0),
      TRUE ~ NA_real_
    ))
  
  
  
  if (!is.null(questionnaire)) {
    all_summary_statistics_labeled <- results %>% lapply(map_to_labeled, 
                                                         questionnaire) %>% lapply(function(x) {
                                                           x$summary.statistic
                                                         }) %>% do.call(rbind, .)
  }
  else {
    all_summary_statistics_labeled <- all_summary_statistics
  }
  if (nrow(all_summary_statistics) < nrow(all_summary_statistics_labeled)) {
    warning("labelising made some analysis definition indistinguishable (identical question labels or same label for different choices in the same question?")
    .write_to_log("mapping resultlist to datamerge csv could not be done correctly with labels - some analysis definitions became indistinguishable ")
  }
  columns <- names(all_summary_statistics)[!(names(all_summary_statistics) %in% 
                                               c(rows, ignore, values))]
  all_summary_statistics_labeled$master_table_column_name <- all_summary_statistics[, 
                                                                                    columns] %>% as.list %>% c(sep = ".") %>% do.call(paste, 
                                                                                                                                      .)
  wide_format <- all_summary_statistics_labeled %>% unique %>% 
    .[, c(rows, "master_table_column_name", values)] %>% 
    spread(key = master_table_column_name, value = numbers)
  return(wide_format)
}

