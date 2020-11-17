#' @description create visualisations for displaying excess mortality
#' @inheritParams get_predictions
#' @inheritParams get_baseline_deaths
#' @param cause_name string; wording to use before the location in the chart
#'   titles
#' @param caption string; wording to use under the chart and ahead of "Source"
#'   notes
#' @param include_covid_ribbon logical; whether to display COVID-19 deaths as a
#'   subset of excess deaths
#' @param facet_fields character vectors up to a length of 2 (or NULL); fields
#'   to aggregate the data to. Possible values are "Deprivation_Quintile",
#'   "Ethnic_Group", "Sex", "Age_Group", "RGN09CD", "UTLAApr19CD", "POD_out",
#'   "name_of_cause"
#' @param to_date date; final date for visualisations
#' @param show_inset logical; display explanatory inset in weekly chart
#'   visualisation
#' @param split_chart string; if facet_fields is not null, field name that
#'   determines how the facets are tabbed. Options are the same as those for
#'   facet_fields. If left blank, then charts are displayed without tabs
#' @param subtitle string; text to display as subtitle in visualisation
#' @param axis_title string; text to display on the y axis of the cumulative
#'   charts and at the top of the left hand column of the table

generate_visualisations <- function(model_filename, visualisation_geography = "england",
                                    from_date = as.Date("2020-03-20"), to_date = ceiling_date(Sys.Date(), unit = "years"), 
                                    directory = Sys.getenv("PREDICTIONS_FILESHARE"),
                                    ucod = NULL, btw_ucod = NULL, ucods = NULL, cod = NULL,
                                    pod = NULL, all_pod = FALSE, all_ucod = FALSE, covid_only = FALSE, deprivation = FALSE,
                                    cause_name = "all cause",
                                    caption = "", include_covid_ribbon = FALSE, ethnicity = FALSE, facet_fields = NULL,
                                    end_date = Sys.Date(), show_inset = FALSE, split_chart = NULL,
                                    subtitle = "", axis_title = "", include_totals = FALSE, age_filter = NULL,
                                    age_group_type = "original") {
  

  # check visualisation_geography inputs ------------------------------------
  geography_variable <- vis_geography_variable(visualisation_geography = visualisation_geography,
                                               ethnicity = ethnicity,
                                               deprivation = deprivation)  
  

  # check category variables ------------------------------------------------
  category_variables <- facet_variables(facet_fields = facet_fields,
                                        ethnicity = ethnicity,
                                        deprivation = deprivation)
  

  # create grouping fields vector -------------------------------------------
  grouping_fields <- c(geography_variable, category_variables, "date")
  
  # check that the variable that charts should be split by is in the facet_fields --------
  if (!is.null(split_chart)) {
    split_chart <- split_chart_variables(split_chart, facet_fields)
  }
  
  # add source to caption
  data_source <- "Source: Public Health England analysis of ONS death registration data"
  caption <- setNames(paste(caption, data_source, sep = "\n"),
                      nm = names(caption))
  
  predictions <- lapply(model_filename,
                        get_predictions, 
                        visualisation_geography = visualisation_geography,
                        from_date = from_date, 
                        to_date = to_date, 
                        directory = directory, 
                        ethnicity = ethnicity, 
                        deprivation = deprivation, 
                        facet_fields = facet_fields,
                        age_filter = age_filter,
                        age_group_type = age_group_type)
  if (all_pod == TRUE) {
    predictions <- map_df(predictions, ~as.data.frame(.x), .id = "POD_out")
  } else if (all_ucod == TRUE) {
    predictions <- map_df(predictions, ~as.data.frame(.x), .id = "name_of_cause")
  } else {
    predictions <- as.data.frame(predictions[[1]])
  }
  
  predictions <- predictions %>%
    mutate(date = as.Date(date))
  
  # get dispersion parameter
  dispersion_parameter <- model_filename %>%
    lapply(filename_model_to_dispersion) %>%
    lapply(readLines)
  if (length(dispersion_parameter) > 1) {
    if (all_pod == TRUE) {
      dispersion_parameter <- map_df(dispersion_parameter, ~as.data.frame(as.numeric(.x)), .id = "POD_out")
    } else if (all_ucod == TRUE) {
      dispersion_parameter <- map_df(dispersion_parameter, ~as.data.frame(as.numeric(.x)), .id = "name_of_cause")
    }
    dispersion_parameter <- tibble::deframe(dispersion_parameter)
    dispersion_parameter <- dispersion_parameter[order(names(dispersion_parameter))]
  } else {
    dispersion_parameter <- dispersion_parameter[[1]] %>%
      as.numeric()
  }
  
  ###------------### get recent deaths data  ###------------###
  registered_deaths <- get_recent_deaths(ucod = ucod, 
                                         btw_ucod = btw_ucod, 
                                         ucods = ucods, 
                                         cod = cod, 
                                         pod = pod,
                                         covid_only = FALSE, 
                                         end_date = end_date + 1,
                                         ethnicity = ethnicity,
                                         deprivation = deprivation,
                                         all_pod = all_pod,
                                         all_ucod = all_ucod,
                                         age_filter = age_filter)
  
  # get holiday dates
  holidays <- timeDate::holidayLONDON(year(min(registered_deaths$Reg_Date)):year(Sys.Date())) #get vector of bank holidays
  holidays <- as.Date(holidays)
  holidays <- replace(holidays, holidays == as.Date("2020-05-04"), as.Date("2020-05-08"))
  
  ## get covid deaths
  covid_deaths <- get_recent_deaths(ucod = ucod, 
                                    btw_ucod = btw_ucod, 
                                    ucods = ucods, 
                                    cod = cod, 
                                    pod = pod,
                                    covid_only = TRUE, 
                                    end_date = end_date + 1,
                                    ethnicity = ethnicity,
                                    deprivation = deprivation,
                                    all_pod = all_pod,
                                    all_ucod = all_ucod,
                                    age_filter = age_filter)
  
  
  # remove first day of series if a bank holiday or weekend
  registered_deaths <- registered_deaths %>%
    adjust_first_date(Reg_Date, holidays) %>%
    adjust_last_date(Reg_Date, holidays)

  covid_deaths <- covid_deaths %>%
    adjust_first_date(Reg_Date, holidays) %>%
    adjust_last_date(Reg_Date, holidays)
  
  ## get utla lookup
  utla_lkp <- utla_lookup()
  
  if (ethnicity == TRUE) {
    if (deprivation == TRUE) {
      ethnicity_deprivation_baseline <- get_denominators(ethnicity = TRUE, 
                                                         deprivation = TRUE) %>%
        rename(RGN09CD = OfficialCode,
               deaths_total = denominator) %>%
        dplyr::select(-month)
      
      ethnicity_proportions <- calculate_ethnicity_proportions(data = ethnicity_deprivation_baseline,
                                                               ethnicity_field = Ethnic_Group,
                                                               region_field = RGN09CD,
                                                               age_field = Age_Group,
                                                               sex_field = Sex,
                                                               deaths_field = deaths_total,
                                                               include_deprivation = TRUE)
      
      registered_deaths <- registered_deaths %>%
        left_join(utla_lkp, by = "UTLAApr19CD") %>%
        group_by(RGN09CD, Ethnic_Group, Deprivation_Quintile, Sex, Age_Group, Reg_Date) %>%
        summarise(deaths_total = sum(deaths_total), .groups = "drop")
      
      covid_deaths <- covid_deaths %>%
        left_join(utla_lkp, by = "UTLAApr19CD") %>%
        group_by(RGN09CD, Ethnic_Group, Deprivation_Quintile, Sex, Age_Group, Reg_Date) %>%
        summarise(deaths_total = sum(deaths_total), .groups = "drop")
      
    } else {
      ethnicity_baseline <- get_denominators(ethnicity = TRUE) %>%
        rename(RGN09CD = OfficialCode,
               deaths_total = denominator) %>%
        dplyr::select(-month)
      ethnicity_proportions <- calculate_ethnicity_proportions(data = ethnicity_baseline,
                                                               ethnicity_field = Ethnic_Group, 
                                                               region_field = RGN09CD, 
                                                               age_field = Age_Group, 
                                                               sex_field = Sex, 
                                                               deaths_field = deaths_total)
      
      
      registered_deaths <- registered_deaths %>%
        left_join(utla_lkp, by = "UTLAApr19CD") %>%
        group_by(RGN09CD, Ethnic_Group, Sex, Age_Group, Reg_Date) %>%
        summarise(deaths_total = sum(deaths_total), .groups = "drop")
      
      covid_deaths <- covid_deaths %>%
        left_join(utla_lkp, by = "UTLAApr19CD") %>%
        group_by(RGN09CD, Ethnic_Group, Sex, Age_Group, Reg_Date) %>%
        summarise(deaths_total = sum(deaths_total), .groups = "drop")
    }
    
    
  } else {
    if (deprivation == TRUE) {
      
      registered_deaths <- registered_deaths %>%
        left_join(utla_lkp, by = "UTLAApr19CD") %>%
        group_by(RGN09CD, Deprivation_Quintile, Sex, Age_Group, Reg_Date) %>%
        summarise(deaths_total = sum(deaths_total), .groups = "drop")
      
      covid_deaths <- covid_deaths %>%
        left_join(utla_lkp, by = "UTLAApr19CD") %>%
        group_by(RGN09CD, Deprivation_Quintile, Sex, Age_Group, Reg_Date) %>%
        summarise(deaths_total = sum(deaths_total), .groups = "drop")
      
    }
  }
  
  ### data checks ###
  registered_death_checks <- pre_processed_death_checks(deaths_data = registered_deaths, 
                                                        utla_lkp = utla_lkp, 
                                                        holidays = holidays, 
                                                        ethnicity = ethnicity,
                                                        deprivation = deprivation, 
                                                        deaths_reallocated = TRUE,
                                                        all_pod = all_pod,
                                                        all_ucod = all_ucod)
  
  covid_death_checks <- pre_processed_death_checks(deaths_data = covid_deaths, 
                                                   utla_lkp = utla_lkp, 
                                                   holidays = holidays, 
                                                   ethnicity = ethnicity,
                                                   deprivation = deprivation, 
                                                   deaths_reallocated = TRUE,
                                                   all_pod = all_pod,
                                                   all_ucod = all_ucod)
  
  age_groups <- age_group_lkp(type = age_group_type) %>%
    pull(Age_Group) %>%
    unique()
  
  ethnic_groups <- c("Asian", "Black", "Mixed", "Other", "White")
  
  ### Preprocess registered deaths
  if (ethnicity == TRUE) {
    if (deprivation == TRUE) {
      registered_deaths <- registered_deaths %>%
        arrange(RGN09CD, Ethnic_Group, Deprivation_Quintile, Sex, Age_Group) %>%
        ethnicity_not_stated_adjustment(proportions = ethnicity_proportions,
                                        ethnicity_field = Ethnic_Group, 
                                        deaths_field = deaths_total, 
                                        region_field = RGN09CD, 
                                        age_field = Age_Group,  
                                        sex_field = Sex, 
                                        date_field = Reg_Date,
                                        include_deprivation = TRUE) %>%
        sex_change_0_1(Sex) %>%
        weekends_to_nearest_work_day(date_field = Reg_Date, 
                                     agg_field = deaths_total) %>%
        bank_hols_to_nearest_work_day(bank_holidays = holidays, 
                                      date_field = Reg_Date, 
                                      agg_field = deaths_total) %>%
        rename(date = Reg_Date,
               registered_count = deaths_total) %>%
        mutate(Age_Group = factor(Age_Group),
               Ethnic_Group = factor(Ethnic_Group),
               Deprivation_Quintile = factor(Deprivation_Quintile))
    } else {
      registered_deaths <- registered_deaths %>%
        arrange(RGN09CD, Ethnic_Group, Sex, Age_Group) %>%
        ethnicity_not_stated_adjustment(proportions = ethnicity_proportions,
                                        ethnicity_field = Ethnic_Group, 
                                        deaths_field = deaths_total, 
                                        region_field = RGN09CD, 
                                        age_field = Age_Group,  
                                        sex_field = Sex, 
                                        date_field = Reg_Date) %>%
        sex_change_0_1(Sex) %>%
        weekends_to_nearest_work_day(date_field = Reg_Date, 
                                     agg_field = deaths_total) %>%
        bank_hols_to_nearest_work_day(bank_holidays = holidays, 
                                      date_field = Reg_Date, 
                                      agg_field = deaths_total) %>%
        rename(date = Reg_Date,
               registered_count = deaths_total) %>%
        mutate(Age_Group = factor(Age_Group),
               Ethnic_Group = factor(Ethnic_Group))
    }
    
  } else {
    if (deprivation  == TRUE) {
      registered_deaths <- registered_deaths %>%
        arrange(RGN09CD, Deprivation_Quintile, Sex, Age_Group, Reg_Date) %>%
        sex_change_0_1(Sex) %>%
        weekends_to_nearest_work_day(date_field = Reg_Date, 
                                     agg_field = deaths_total) %>%
        bank_hols_to_nearest_work_day(bank_holidays = holidays, 
                                      date_field = Reg_Date, 
                                      agg_field = deaths_total) %>%
        rename(date = Reg_Date,
               registered_count = deaths_total) %>%
        mutate(Age_Group = factor(Age_Group),
               Deprivation_Quintile = factor(Deprivation_Quintile))
    } else if (deprivation == FALSE) {
      registered_deaths <- registered_deaths %>%
        aggregate_Scillies_CoL(UTLAApr19CD, deaths_total)
      
      if (all_pod == TRUE) {
        registered_deaths <- registered_deaths %>%
          arrange(UTLAApr19CD, POD_out, Sex, Age_Group, Reg_Date)
      } else if (all_ucod == TRUE) {
        registered_deaths <- registered_deaths %>%
          arrange(UTLAApr19CD, name_of_cause, Sex, Age_Group, Reg_Date)
      } else {
        registered_deaths <- registered_deaths %>%
          arrange(UTLAApr19CD, Sex, Age_Group, Reg_Date)
      }
      
      registered_deaths <- registered_deaths %>%
        sex_change_0_1(Sex) %>%
        weekends_to_nearest_work_day(date_field = Reg_Date, 
                                     agg_field = deaths_total) %>%
        bank_hols_to_nearest_work_day(bank_holidays = holidays, 
                                      date_field = Reg_Date, 
                                      agg_field = deaths_total) %>%
        rename(date = Reg_Date,
               registered_count = deaths_total) %>%
        mutate(Age_Group = factor(Age_Group))
        
    }
    
  }
  
  ### Preprocess covid deaths
  if (ethnicity == TRUE) {
    if (deprivation == TRUE) {
      covid_deaths <- covid_deaths %>%
        arrange(RGN09CD, Ethnic_Group, Deprivation_Quintile, Sex, Age_Group) %>%
        ethnicity_not_stated_adjustment(proportions = ethnicity_proportions,
                                        ethnicity_field = Ethnic_Group, 
                                        deaths_field = deaths_total, 
                                        region_field = RGN09CD, 
                                        age_field = Age_Group,  
                                        sex_field = Sex, 
                                        date_field = Reg_Date,
                                        include_deprivation = TRUE) %>%
        sex_change_0_1(Sex) %>%
        weekends_to_nearest_work_day(date_field = Reg_Date, 
                                     agg_field = deaths_total) %>%
        bank_hols_to_nearest_work_day(bank_holidays = holidays, 
                                      date_field = Reg_Date, 
                                      agg_field = deaths_total) %>%
        rename(date = Reg_Date) %>%
        mutate(Age_Group = factor(Age_Group),
               Ethnic_Group = factor(Ethnic_Group),
               Deprivation_Quintile = factor(Deprivation_Quintile))
    } else {
      covid_deaths <- covid_deaths %>%
        arrange(RGN09CD, Ethnic_Group, Sex, Age_Group) %>%
        ethnicity_not_stated_adjustment(proportions = ethnicity_proportions,
                                        ethnicity_field = Ethnic_Group, 
                                        deaths_field = deaths_total, 
                                        region_field = RGN09CD, 
                                        age_field = Age_Group,  
                                        sex_field = Sex, 
                                        date_field = Reg_Date) %>%
        sex_change_0_1(Sex) %>%
        weekends_to_nearest_work_day(date_field = Reg_Date, 
                                     agg_field = deaths_total) %>%
        bank_hols_to_nearest_work_day(bank_holidays = holidays, 
                                      date_field = Reg_Date, 
                                      agg_field = deaths_total) %>%
        rename(date = Reg_Date) %>%
        mutate(Age_Group = factor(Age_Group),
               Ethnic_Group = factor(Ethnic_Group))
    }
    
  } else {
    if (deprivation == TRUE) {
      covid_deaths <- covid_deaths %>%
        arrange(RGN09CD, Deprivation_Quintile, Sex, Age_Group, Reg_Date) %>%
        sex_change_0_1(Sex) %>%
        weekends_to_nearest_work_day(date_field = Reg_Date, 
                                     agg_field = deaths_total) %>%
        bank_hols_to_nearest_work_day(bank_holidays = holidays, 
                                      date_field = Reg_Date, 
                                      agg_field = deaths_total) %>%
        rename(date = Reg_Date) %>%
        mutate(Age_Group = factor(Age_Group),
               Deprivation_Quintile = factor(Deprivation_Quintile))
    } else if (deprivation == FALSE) {
      covid_deaths <- covid_deaths %>%
        aggregate_Scillies_CoL(UTLAApr19CD, deaths_total)
      
      if (all_pod == TRUE) {
        covid_deaths <- covid_deaths %>%
          arrange(UTLAApr19CD, POD_out, Sex, Age_Group, Reg_Date)
      } else if (all_ucod == TRUE) {
        covid_deaths <- covid_deaths %>%
          arrange(UTLAApr19CD, name_of_cause, Sex, Age_Group, Reg_Date)
      } else {
        covid_deaths <- covid_deaths %>%
          arrange(UTLAApr19CD, Sex, Age_Group, Reg_Date)
      }
      
      covid_deaths <- covid_deaths %>%
        sex_change_0_1(Sex) %>%
        weekends_to_nearest_work_day(date_field = Reg_Date, 
                                     agg_field = deaths_total) %>%
        bank_hols_to_nearest_work_day(bank_holidays = holidays, 
                                      date_field = Reg_Date, 
                                      agg_field = deaths_total) %>%
        rename(date = Reg_Date) %>%
        mutate(Age_Group = factor(Age_Group))
        
    }
    
  }
  
  ### data checks ###
  deaths_registered_post_checks <- post_processed_death_checks(registered_deaths,
                                                               holidays = holidays,
                                                               total_deaths = registered_death_checks$total_deaths,
                                                               ethnicity = ethnicity,
                                                               deprivation = deprivation,
                                                               total_deaths_around_hols_weekends = registered_death_checks$total_deaths_around_hols_weekends,
                                                               deaths_reallocated = TRUE,
                                                               days_reallocated = FALSE,
                                                               deaths_field = registered_count)
  deaths_covid_post_checks <- post_processed_death_checks(covid_deaths,
                                                          holidays = holidays,
                                                          total_deaths = covid_death_checks$total_deaths,
                                                          ethnicity = ethnicity,
                                                          deprivation = deprivation,
                                                          total_deaths_around_hols_weekends = covid_death_checks$total_deaths_around_hols_weekends,
                                                          deaths_reallocated = TRUE,
                                                          days_reallocated = FALSE,
                                                          deaths_field = deaths_total)
  
  
  
  if (ethnicity != TRUE & deprivation != TRUE) {
    registered_deaths <- registered_deaths %>% 
      left_join(utla_lkp, by = "UTLAApr19CD")
    
    covid_deaths <- covid_deaths %>% 
      left_join(utla_lkp, by = "UTLAApr19CD")
  }
  registered_deaths <- registered_deaths %>% 
    group_by(across(all_of(grouping_fields))) %>%
    summarise(registered_count = sum(registered_count),
              .groups = "keep") %>%
    ungroup()
  
  covid_deaths <- covid_deaths %>% 
    group_by(across(all_of(grouping_fields))) %>%
    summarise(deaths_total = sum(deaths_total),
              .groups = "keep") %>%
    ungroup()
  
  # chart_data is full predications and registereds by UTLA age and sex
  chart_data <- predictions
  
  if ("Deprivation_Quintile" %in% names(chart_data)) {
    chart_data <- chart_data %>% 
      mutate(Deprivation_Quintile = factor(Deprivation_Quintile))
  }
  
  if ("Ethnic_Group" %in% names(chart_data)) {
    chart_data <- chart_data %>% 
      mutate(Ethnic_Group = factor(Ethnic_Group))
    
  }
  
  if ("Sex" %in% names(chart_data)) {
    chart_data <- chart_data %>% 
      mutate(Sex = factor(Sex))
  }
  
  if ("Age_Group" %in% names(chart_data)) {
    chart_data <- chart_data %>% 
      mutate(Age_Group = factor(Age_Group))
  }
  
  chart_data <- chart_data %>% 
    full_join(registered_deaths, by = grouping_fields) %>%
    full_join(covid_deaths, by = grouping_fields)
  
  if (visualisation_geography == "region") {
    region_lkp <- utla_lkp %>% 
      distinct(RGN09CD, RGN09NM)
    
    chart_data <- chart_data %>% 
      left_join(region_lkp, by = "RGN09CD")
    
  }
  
  if ("Deprivation_Quintile" %in% names(chart_data)) {
    chart_data <- chart_data %>% 
      transform_deprivation_groups(Deprivation_Quintile)
  }
    
  if ("Ethnic_Group" %in% names(chart_data)) {
    chart_data <- chart_data %>% 
      transform_ethnic_groups(Ethnic_Group, ethnic_groups, apply_labels = FALSE)
    
  }
  
  chart_data <- chart_data %>% 
    filter(date >= from_date) %>%
    mutate(modelled_deaths_zeros = ifelse(is.na(modelled_deaths), 0, modelled_deaths)) %>%
    arrange(across(all_of(grouping_fields)))
  
  if ("Sex" %in% names(chart_data)) {
    chart_data <- chart_data %>% 
      transform_sex_groups(Sex)
  }
  
  if ("Age_Group" %in% names(chart_data)) {
    chart_data <- chart_data %>% 
      transform_age_groups(Age_Group, age_groups, apply_labels = FALSE)
  }
  
  if ("RGN09CD" %in% facet_fields) {
    rgn_lkp <- utla_lkp %>%
      distinct(RGN09CD, RGN09NM)
    
    chart_data <- chart_data %>%
      left_join(rgn_lkp, by = "RGN09CD")
    
    facet_fields[facet_fields == "RGN09CD"] <- "RGN09NM"
    
    if (!is.null(split_chart)) {
      if (split_chart == "RGN09CD") {
        split_chart <- "RGN09NM"
      } 
    }
  }
  
  if ("RGN09NM" %in% names(chart_data)) {
    chart_data <- chart_data %>% 
      transform_regions(RGN09NM)
  }
  
  if ("UTLAApr19CD" %in% facet_fields) {
    utla_nm_lkp <- utla_lkp %>%
      distinct(UTLAApr19CD, UTLAApr19NM)
    
    chart_data <- chart_data %>%
      left_join(utla_nm_lkp, by = "UTLAApr19CD")
    
    facet_fields[facet_fields == "UTLAApr19CD"] <- "UTLAApr19NM"
    
    if (!is.null(split_chart)) {
      if (split_chart == "UTLAApr19CD") {
        split_chart <- "UTLAApr19NM"
      } 
    }
  }
  
  if (visualisation_geography == "england") {
    chart_data <- chart_data %>% 
      mutate(area_name = "England")
  } else if (visualisation_geography == "region") {
    chart_data <- chart_data %>% 
      mutate(area_name = RGN09NM)
  } else if (visualisation_geography == "utla") {
    chart_data <- chart_data %>% 
      mutate(area_name = UTLAApr19NM)
  }
  
  if (all_ucod == TRUE) {
    cause_levels <- model_references() %>% 
      filter(!is.na(reference),
             caption != "") %>% 
      pull(reference) %>% 
      rev()
    
    chart_data <- chart_data %>% 
      mutate(name_of_cause = factor(name_of_cause,
                                    levels = cause_levels))
  }

  if (include_totals == TRUE) {
    if (!is.null(split_chart)) {
    grouping_fields_totals <- c(split_chart, "date")
    grouping_fields_missing <- setdiff(grouping_fields, grouping_fields_totals)
    new_levels <- c(levels(chart_data[[grouping_fields_missing]]),"Total")
    
    chart_data_totals <- chart_data %>%
      group_by(across(all_of(grouping_fields_totals))) %>%
      summarise(registered_count = sum(registered_count),
                deaths_total = sum(deaths_total),
                modelled_deaths = sum(modelled_deaths),
                .groups = "keep") %>%
      ungroup() %>%
      mutate(!! rlang::sym(grouping_fields_missing) := "Total",
             modelled_deaths_zeros = ifelse(is.na(modelled_deaths), 0,
                                            modelled_deaths),
             area_name = "England") 
      
    
    chart_data <- chart_data %>%
      bind_rows(chart_data_totals) %>%
      mutate(!! rlang::sym(grouping_fields_missing) := factor(!! rlang::sym(grouping_fields_missing),
                                                              levels = new_levels))
    }
    else {
      warning ("split_chart needs to be populated in order for totals to be calculated.")
    }
  }
  
  #### BEGIN PLOTTING ####
  
  excessdeaths_chart <- list()
  weekly_chart_simple <- list()
  final_cumulative <- list()
  
  export_powerbi_data(data = chart_data, 
                      grouping_fields = grouping_fields,
                      model_filename = gsub("model_outputs/", "", gsub("\\.rds", "", model_filename)), 
                      end_date = end_date + 1,
                      age_filter = age_filter)
  
  for (area in unique(chart_data$area_name)) {
    chart_data_filtered <- chart_data %>% 
      filter(area_name == area)
      
    if (is.null(facet_fields)) {
      if (!is.null(pod)) {
        pod_subtitle <- pod_lookup() %>%
          filter(pod_input %in% pod) %>%
          pull(pod_filter) %>%
          tolower()
        
        pod_subtitle[pod_subtitle == "home"] <- paste("at", pod_subtitle[pod_subtitle == "home"])
        pod_subtitle[grepl("care|hosp", pod_subtitle)] <- paste("in a", pod_subtitle[grepl("care|hosp", pod_subtitle)])
        pod_subtitle[grepl("other", pod_subtitle)] <- paste("in", pod_subtitle[grepl("other", pod_subtitle)])
        
        
        pod_subtitle <- paste("among deaths occurring", 
                              paste(pod_subtitle, collapse = ", "))
      } else {
        pod_subtitle <- ""
      }
      
      excessdeaths_chart[[area]] <- cumulative_excess_deaths(death_data = chart_data_filtered,
                                                             cause_name = cause_name,
                                                             area_name = paste(area, pod_subtitle, sep = "\n"),
                                                             date_field = date,
                                                             model_field = modelled_deaths_zeros,
                                                             deaths_field = registered_count,
                                                             covid_field = deaths_total,
                                                             start_date = from_date,
                                                             include_text_boxes = TRUE,
                                                             include_covid_ribbon = include_covid_ribbon,
                                                             caption = caption,
                                                             end_date = end_date)
      
      weekly_chart_simple[[area]] <- weekly_deaths_simple(death_data = chart_data_filtered,
                                                          area_name = paste(area, pod_subtitle, sep = "\n"),
                                                          cause_name = cause_name,
                                                          date_field = date,
                                                          model_field = modelled_deaths_zeros,
                                                          deaths_field = registered_count,
                                                          covid_field = deaths_total,
                                                          start_date = from_date,
                                                          end_date = end_date,
                                                          show_inset = show_inset,
                                                          caption = caption)
      
      
    } else if (length(facet_fields) == 1) {
      if (is.null(split_chart)) {
        weekly_chart_simple[[area]] <- weekly_deaths_simple_facetted(death_data = chart_data_filtered,
                                                                     area_name = area,
                                                                     cause_name = cause_name,
                                                                     date_field = date,
                                                                     model_field = modelled_deaths_zeros,
                                                                     deaths_field = registered_count,
                                                                     facet_field = facet_fields,
                                                                     covid_field = deaths_total,
                                                                     start_date = from_date,
                                                                     end_date = end_date,
                                                                     subtitle = subtitle,
                                                                     caption = caption)
      } else {
        
        splitting_items <- chart_data_filtered %>%
          pull(split_chart) %>%
          unique() %>% 
          as.character()
        
        if (split_chart %in% c("RGN09NM", "UTLAApr19NM")) {
          area_name <- setNames(splitting_items,
                                nm = splitting_items)
        } else {
          area_name <- setNames(rep(area, length(splitting_items)),
                                nm = splitting_items)
        } 
        
        if (all_ucod == TRUE) {
          cause_name <- setNames(splitting_items,
                                 nm = splitting_items)
        } else if (all_pod == TRUE) {
          title_part <- gsub("^home$", "own home", tolower(splitting_items))
          cause_name <- setNames(title_part,
                                 nm = splitting_items)
        } else {
          cause_name <- setNames(rep("all cause", length(splitting_items)),
                                 nm = splitting_items)
        }
        
        if (length(caption) == 1) caption <- setNames(rep(caption, length(splitting_items)),
                                                      nm = splitting_items)
        
        weekly_chart_simple[[area]] <- splitting_items %>%
          lapply(function(x) weekly_deaths_simple(death_data = chart_data_filtered %>% filter(!! sym(split_chart) == x),
                                                  area_name = area_name[x],
                                                  cause_name = cause_name[x],
                                                  date_field = date,
                                                  model_field = modelled_deaths_zeros,
                                                  deaths_field = registered_count,
                                                  covid_field = deaths_total,
                                                  start_date = from_date,
                                                  end_date = end_date,
                                                  subtitle = subtitle,
                                                  caption = caption[x]))
        
        names(weekly_chart_simple[[area]]) <- splitting_items
        
      }
      
      caption <- paste("", data_source, sep = "\n")
      
      final_cumulative[[area]] <- cumulative_compare(death_data = chart_data_filtered,
                                                     area_name = area,
                                                     date_field = date, 
                                                     model_field = modelled_deaths_zeros, 
                                                     deaths_field = registered_count,
                                                     covid_field = deaths_total,
                                                     axis_field = facet_fields,
                                                     start_date = from_date,
                                                     subtitle = subtitle,
                                                     end_date = end_date,
                                                     axis_title = axis_title,
                                                     caption = caption,
                                                     dispersion_parameter = dispersion_parameter)
      
      
    } else if (length(facet_fields) == 2) {
      splitting_items <- chart_data_filtered %>%
        pull(split_chart) %>%
        unique()
      
      facet_var <- setdiff(facet_fields, split_chart)
      
      cause_name <- setNames(tolower(splitting_items),
                             nm = splitting_items)
      
      weekly_chart_simple[[area]] <- splitting_items %>%
        lapply(function(x) weekly_deaths_simple_facetted(death_data = chart_data_filtered %>% filter(!! sym(split_chart) == x),
                                                         area_name = area,
                                                         cause_name = cause_name[x],
                                                         date_field = date,
                                                         model_field = modelled_deaths_zeros,
                                                         deaths_field = registered_count,
                                                         facet_field = facet_var,
                                                         covid_field = deaths_total,
                                                         start_date = from_date,
                                                         end_date = end_date,
                                                         subtitle = paste0(subtitle, " (", x, ")"),
                                                         caption = caption))
      
      names(weekly_chart_simple[[area]]) <- splitting_items
      
      final_cumulative[[area]] <- splitting_items %>%
        lapply(function(x) cumulative_compare(death_data = chart_data_filtered[chart_data_filtered[[split_chart]] == x, ],
                                              area_name = area,
                                              date_field = date, 
                                              model_field = modelled_deaths_zeros, 
                                              deaths_field = registered_count,
                                              covid_field = deaths_total,
                                              axis_field = facet_var,
                                              start_date = from_date,
                                              subtitle = paste0(subtitle, " (", x, ")"),
                                              end_date = end_date,
                                              axis_title = axis_title,
                                              caption = caption,
                                              dispersion_parameter = dispersion_parameter))
      names(final_cumulative[[area]]) <- splitting_items
       
    }
    
  }
  
  return(list(weekly_simple = weekly_chart_simple,
              cumulative = excessdeaths_chart,
              cumulative_compare = final_cumulative))
 
}
