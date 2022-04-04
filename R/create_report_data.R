generate_report_data <- function(final_date, eth_dep_setting,
                                 age_group_type_setting,
                                 pop_type_setting, from_date,
                                 report_type = "live") {
  
  print("creating england data")
  eng_charts <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                        visualisation_geography = "england",
                                        end_date = final_date, 
                                        from_date = from_date,
                                        eth_dep = eth_dep_setting, 
                                        age_group_type = age_group_type_setting,
                                        report_type = report_type)
  
  print("creating age-sex data")
  eng_as_charts <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                           visualisation_geography = "england",
                                           facet_fields = c("Age_Group", "Sex"),
                                           split_chart = "Sex",
                                           end_date = final_date, 
                                           from_date = from_date,
                                           eth_dep = eth_dep_setting, 
                                           include_totals = TRUE,
                                           age_group_type = age_group_type_setting,
                                           report_type = report_type)
  
  print("creating regions data")
  region_charts <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                           visualisation_geography = "england", 
                                           facet_fields = "RGN09CD",
                                           split_chart = "RGN09CD",
                                           end_date = final_date,
                                           from_date = from_date,
                                           eth_dep = eth_dep_setting, 
                                           age_group_type = age_group_type_setting,
                                           report_type = report_type)
  
  #### ETHNICITY CHARTS ####
  print("creating ethnicity-sex data")
  england_ethnicity_sex <- generate_visualisations(model_filename =  paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                                   eth_dep = eth_dep_setting, 
                                                   visualisation_geography = "england",
                                                   facet_fields = c("Ethnic_Group", "Sex"), 
                                                   split_chart = "Sex",
                                                   end_date = ethnic_group_final_date, 
                                                   from_date = from_date,
                                                   age_group_type = age_group_type_setting,
                                                   report_type = report_type)
  
  #### DEPRIVATION CHARTS ####
  print("creating deprivation sex data")
  england_deprivation_sex <- generate_visualisations(model_filename = paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                                     eth_dep = eth_dep_setting,
                                                     visualisation_geography = "england",
                                                     facet_fields = c("Deprivation_Quintile", "Sex"),
                                                     split_chart = "Sex",
                                                     end_date = final_date,
                                                     from_date = from_date,
                                                     age_group_type = age_group_type_setting,
                                                     report_type = report_type)
 
  # UTLA --------------------------------------------------------------------
  print("creating utla data")
  utla_charts <- generate_visualisations(paste0("model_outputs/", all_models("UTLA"), ".rds"),
                                         visualisation_geography = "england",
                                         facet_fields = "UTLAApr19CD",
                                         split_chart = "UTLAApr19CD",
                                         end_date = final_date,
                                         from_date = from_date,
                                         eth_dep = FALSE,
                                         report_type = report_type)
  
  #### MORTALITY BY CAUSE OF DEATH ####
  
  # all diseases compared
  print("creating cause of death data")
  causes <- cause_code_lookup(table_output = TRUE,
                              include_epi_hf = TRUE) %>%
    filter(name_of_cause != "COVID-19") %>%
    mutate(name_of_cause = capitalise_first_letter(name_of_cause)) %>% 
    group_by(name_of_cause) %>% 
    summarise(codes = list(ICD_codes),
              .groups = "drop")
  
  model_files <- model_references() %>%
    filter(reference %in% causes$name_of_cause) %>%
    dplyr::select(reference, model_file) %>% 
    left_join(causes, by = c("reference" = "name_of_cause")) %>% 
    mutate(model_file = gsub("\\\\", "/", model_file))
  
  all_mentions_data <- model_files %>% 
    group_by(reference) %>% 
    group_split() %>% 
    purrr::map(~ generate_visualisations(ucods = .x[["codes"]][[1]],
                                         cod = .x[["codes"]][[1]],
                                         model_filename = .x[["model_file"]],
                                         end_date = final_date,
                                         from_date = from_date,
                                         visualisation_geography = "england",
                                         report_type = report_type))
  
  #### MORTALITY BY PLACE OF DEATH ####
  # All places of death combined
  print("creating place of death data")
  pods <- pod_lookup() %>%
    pull(pod_filter) %>%
    unique()
  model_files <- model_references() %>%
    filter(reference %in% pods) %>%
    dplyr::select(reference, model_file) %>%
    tibble::deframe()
  
  all_pods <- generate_visualisations(model_filename = model_files,
                                      all_pod = TRUE,
                                      end_date = final_date,
                                      from_date = from_date,
                                      visualisation_geography = "england",
                                      facet_fields = "POD_out",
                                      split_chart = "POD_out",
                                      report_type = report_type)
  
  
  # check that all objects exist --------------------------------------------
  expected_objects <- c("eng_charts",
                        "eng_as_charts",
                        "region_charts",
                        "utla_charts",
                        "england_deprivation_sex",
                        "england_ethnicity_sex",
                        "all_mentions_data",
                        "all_pods")
  objects_that_are_here <- sapply(expected_objects, 
                                  exists, 
                                  envir = environment())
  is_everything_here <- all(objects_that_are_here)
  
  return(list(is_everything_here = is_everything_here,
              objects_that_are_here = objects_that_are_here))
}