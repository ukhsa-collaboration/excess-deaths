generate_report_data <- function(final_date, eth_dep_setting,
                                 age_group_type_setting,
                                 pop_type_setting, from_date) {
  
  print("creating england data")
  eng_charts <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                        visualisation_geography = "england",
                                        include_covid_ribbon = TRUE, 
                                        end_date = final_date, 
                                        from_date = from_date,
                                        eth_dep = eth_dep_setting, 
                                        show_inset = TRUE,
                                        age_group_type = age_group_type_setting,
                                        stop_before_visualisation = TRUE)
  
  print("creating age-sex data")
  eng_as_charts <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                           visualisation_geography = "england",
                                           facet_fields = c("Age_Group", "Sex"),
                                           split_chart = "Sex",
                                           include_covid_ribbon = TRUE, 
                                           end_date = final_date, 
                                           from_date = from_date,
                                           eth_dep = eth_dep_setting, 
                                           axis_title = "Age group (years)",
                                           include_totals = TRUE,
                                           age_group_type = age_group_type_setting,
                                           stop_before_visualisation = TRUE)
  
  print("creating regions data")
  region_charts <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                           visualisation_geography = "england", 
                                           facet_fields = "RGN09CD",
                                           split_chart = "RGN09CD",
                                           end_date = final_date,
                                           from_date = from_date,
                                           include_covid_ribbon = TRUE, 
                                           eth_dep = eth_dep_setting, 
                                           axis_title = "Region",
                                           age_group_type = age_group_type_setting,
                                           stop_before_visualisation = TRUE)
  
  #### ETHNICITY CHARTS ####
  print("creating ethnicity-sex data")
  england_ethnicity_sex <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                                   eth_dep = eth_dep_setting, 
                                                   visualisation_geography = "england",
                                                   facet_fields = c("Ethnic_Group", "Sex"), 
                                                   split_chart = "Sex",
                                                   end_date = ethnic_group_final_date, 
                                                   from_date = from_date,
                                                   include_covid_ribbon = TRUE, 
                                                   axis_title = "Ethnic group",
                                                   age_group_type = age_group_type_setting,
                                                   # age_filter = c(0, 74),
                                                   stop_before_visualisation = TRUE)
  
  #### DEPRIVATION CHARTS ####
  # england_eth_dep <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
  #                                            eth_dep = eth_dep_setting, 
  #                                            visualisation_geography = "england",
  #                                            facet_fields = c("Deprivation_Quintile", "Ethnic_Group"),
  #                                            split_chart = "Ethnic_Group",
  #                                            end_date = final_date, 
  #                                            from_date = from_date,
  #                                            include_covid_ribbon = TRUE, 
  #                                            axis_title  = "Deprivation quintile",
  #                                            age_group_type = age_group_type_setting,
  #                                            age_filter = c(0, 74),
  #                                            stop_before_visualisation = TRUE)
  
  print("creating deprivation data")
  england_deprivation <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                                 eth_dep = eth_dep_setting, 
                                                 visualisation_geography = "england",
                                                 facet_fields = "Deprivation_Quintile", 
                                                 split_chart = "Deprivation_Quintile",
                                                 end_date = final_date, 
                                                 from_date = from_date,
                                                 include_covid_ribbon = TRUE, 
                                                 axis_title  = "Deprivation quintile",
                                                 age_group_type = age_group_type_setting,
                                                 # age_filter = c(0, 74),
                                                 stop_before_visualisation = TRUE)
  
  # england_deprivation_u75 <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
  #                                                    eth_dep = eth_dep_setting, 
  #                                                    visualisation_geography = "england",
  #                                                    facet_fields = "Deprivation_Quintile", 
  #                                                    split_chart = "Deprivation_Quintile",
  #                                                    end_date = final_date, 
  #                                                    from_date = from_date,
  #                                                    include_covid_ribbon = TRUE, 
  #                                                    axis_title  = "Deprivation quintile",
  #                                                    age_group_type = age_group_type_setting,
  #                                                    age_filter = c(0, 74),
  #                                            stop_before_visualisation = TRUE))
  # 
  # england_deprivation_o75 <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
  #                                                    eth_dep = eth_dep_setting, 
  #                                                    visualisation_geography = "england",
  #                                                    facet_fields = "Deprivation_Quintile", 
  #                                                    split_chart = "Deprivation_Quintile",
  #                                                    end_date = final_date, 
  #                                                    from_date = from_date,
  #                                                    include_covid_ribbon = TRUE, 
  #                                                    axis_title  = "Deprivation quintile",
  #                                                    age_group_type = age_group_type_setting,
  #                                                    age_filter = c(75, NA),
  #                                            stop_before_visualisation = TRUE))
  
  
  # UTLA --------------------------------------------------------------------
  print("creating utla data")
  utla_charts <- generate_visualisations(paste0("model_outputs/", all_models("UTLA"), ".rds"),
                                         visualisation_geography = "england",
                                         facet_fields = "UTLAApr19CD",
                                         split_chart = "UTLAApr19CD",
                                         end_date = final_date,
                                         from_date = from_date,
                                         include_covid_ribbon = TRUE,
                                         eth_dep = FALSE,
                                         axis_title = "Upper Tier Local Authority",
                                         stop_before_visualisation = TRUE)
  
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
                                         cause_name = "",
                                         include_covid_ribbon = FALSE,
                                         end_date = final_date,
                                         from_date = from_date,
                                         visualisation_geography = "england",
                                         stop_before_visualisation = TRUE))
  
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
                                      include_covid_ribbon = TRUE,
                                      axis_title = "Place of death",
                                      stop_before_visualisation = TRUE)
  
  
  # check that all objects exist --------------------------------------------
  expected_objects <- c("eng_charts",
                        "eng_as_charts",
                        "region_charts",
                        "utla_charts",
                        "england_deprivation",
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