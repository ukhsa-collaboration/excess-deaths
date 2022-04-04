#' @description create visualisations for displaying excess mortality
#' @inheritParams get_predictions
#' @inheritParams get_baseline_deaths
#' @param eth_dep logical; whether ethnicity and deprivation were included in
#'   the model the visualisation is based on
#' @param facet_fields character vectors up to a length of 2 (or NULL); fields
#'   to aggregate the data to. Possible values are "Deprivation_Quintile",
#'   "Ethnic_Group", "Sex", "Age_Group", "RGN09CD", "UTLAApr19CD", "POD_out",
#'   "name_of_cause"
#' @param to_date date; date passed to get_predictions. This date acts as the
#'   final date that predictions are required for. If predictions do not exist
#'   up to that date, they are generated and combined with previously calculated
#'   predictions
#' @param split_chart string; if facet_fields is not null, field name that
#'   determines how the facets are tabbed. Options are the same as those for
#'   facet_fields. If left blank, then charts are displayed without tabs
#' @param include_totals logical; whether to include a group for totals in the
#'   age-sex visualisations
#' @param age_filter numeric vector (length 2); an upper and lower threshold to
#'   truncate the age range for analysis. Can use NA for either value if ages up
#'   to the limit should be considered (ie, c(50, NA) will consider ages between
#'   50 and the maximum age in the dataset)
#' @param age_group_type string; "original", "nomis", "bespoke" are accepted
#'   inputs. If "bespoke" is used, then the bespoke_age_groups parameter needs
#'   to be provided. "bespoke" option is currently incompatible with eth_dep =
#'   TRUE because baseline data comes from pre-calculated file
#' @param bespoke_age_groups tibble; two fields - Age and Age_Group. Age
#'   contains integers and Age_Group is a character class displaying the age
#'   group that that age is assigned to
#' @param report_type string; "live" or "test" - if it is test then all files
#'   that are generated will be suffixed with "test" so they don't overwrite any
#'   of the live files
#'   

generate_visualisations <- function(model_filename, visualisation_geography = "england",
                                    from_date = as.Date("2020-03-20"), to_date = ceiling_date(Sys.Date(), unit = "years") - 1, 
                                    directory = Sys.getenv("PREDICTIONS_FILESHARE"),
                                    ucod = NULL, ucods = NULL, cod = NULL,
                                    pod = NULL, all_pod = FALSE, covid_only = FALSE, 
                                    eth_dep = FALSE, facet_fields = NULL,
                                    end_date = Sys.Date(), split_chart = NULL,
                                    include_totals = FALSE, age_filter = NULL,
                                    age_group_type = "original", 
                                    bespoke_age_groups = NULL,
                                    report_type = "live") {
  

  # check report_type input -------------------------------------------------
  report_type <- match.arg(report_type, c("live", "test"))
  
  # check visualisation_geography inputs ------------------------------------
  geography_variable <- vis_geography_variable(visualisation_geography = visualisation_geography,
                                               eth_dep = eth_dep)  
  

  # check category variables ------------------------------------------------
  category_variables <- facet_variables(facet_fields = facet_fields,
                                        eth_dep = eth_dep)
  

  # create grouping fields vector -------------------------------------------
  grouping_fields <- c(geography_variable, category_variables, "date")
  
  # check that the variable that charts should be split by is in the facet_fields --------
  if (!is.null(split_chart)) {
    split_chart <- split_chart_variables(split_chart, facet_fields)
  }
  
  # should specific causes be broken down by ucod
  breakdown_by_ucod <- FALSE
  if (!is.null(cod) & 
      any(!is.null(ucod), !is.null(ucods))) breakdown_by_ucod <- TRUE
  
  predictions <- lapply(model_filename,
                        get_predictions, 
                        visualisation_geography = visualisation_geography,
                        from_date = from_date, 
                        to_date = to_date, 
                        directory = directory, 
                        eth_dep = eth_dep, 
                        facet_fields = facet_fields,
                        age_filter = age_filter,
                        age_group_type = age_group_type,
                        bespoke_age_groups = bespoke_age_groups)
  if (all_pod == TRUE) {
    predictions <- map_df(predictions, ~as.data.frame(.x), .id = "POD_out")
  } else {
    predictions <- as.data.frame(predictions[[1]])
  }
  # ****added in for predictions run for sex & deprivation**** #----
  # ****TO REMOVE**** #----
  # return("complete")
  
  predictions <- predictions %>%
    mutate(date = as.Date(date))
  
  # get dispersion parameter
  dispersion_parameter <- model_filename %>%
    lapply(filename_model_to_dispersion) %>%
    lapply(readLines)
  if (length(dispersion_parameter) > 1) {
    if (all_pod == TRUE) {
      dispersion_parameter <- map_df(dispersion_parameter, ~as.data.frame(as.numeric(.x)), .id = "POD_out")
    }
    
    dispersion_parameter <- tibble::deframe(dispersion_parameter)
    dispersion_parameter <- dispersion_parameter[order(names(dispersion_parameter))]
  } else {
    dispersion_parameter <- dispersion_parameter[[1]] %>%
      as.numeric()
  }
  
  ###------------### get recent deaths data  ###------------###
  # get ucod deaths if displaying deaths with specific mentions
  if (breakdown_by_ucod == TRUE) {
    # deaths with a mention of specific cause
    registered_deaths <- get_recent_deaths(cod = cod, 
                                           pod = pod,
                                           covid_only = FALSE, 
                                           end_date = end_date,
                                           eth_dep = eth_dep,
                                           all_pod = all_pod,
                                           age_filter = age_filter,
                                           age_group_type = age_group_type,
                                           bespoke_age_groups = bespoke_age_groups)
    
    # deaths with underlying cause
    ucod_deaths <- get_recent_deaths(ucod = ucod, 
                                     ucods = ucods, 
                                     pod = pod,
                                     covid_only = FALSE, 
                                     end_date = end_date,
                                     eth_dep = eth_dep,
                                     all_pod = all_pod,
                                     age_filter = age_filter,
                                     age_group_type = age_group_type,
                                     bespoke_age_groups = bespoke_age_groups)
  } else {
    registered_deaths <- get_recent_deaths(ucod = ucod, 
                                           ucods = ucods, 
                                           cod = cod, 
                                           pod = pod,
                                           covid_only = FALSE, 
                                           end_date = end_date,
                                           eth_dep = eth_dep,
                                           all_pod = all_pod,
                                           age_filter = age_filter,
                                           age_group_type = age_group_type,
                                           bespoke_age_groups = bespoke_age_groups)
  }
  
  # get holiday dates
  holidays <- holiday_dates(from_date = min(registered_deaths$Reg_Date),
                            to_date = Sys.Date()) #get vector of bank holidays
  
  ## get covid deaths
  covid_deaths <- get_recent_deaths(ucod = ucod, 
                                    ucods = ucods, 
                                    cod = cod, 
                                    pod = pod,
                                    covid_only = TRUE, 
                                    end_date = end_date,
                                    eth_dep = eth_dep,
                                    all_pod = all_pod,
                                    age_filter = age_filter,
                                    age_group_type = age_group_type,
                                    bespoke_age_groups = bespoke_age_groups)
  
  
  if (all_pod){
    registered_deaths <- registered_deaths %>%
      filter(!is.na(POD_out))
    
    covid_deaths <- covid_deaths %>%
      filter(!is.na(POD_out))
    
  }
  
  
  ## get utla lookup
  utla_lkp <- utla_lookup()
  
  if (eth_dep == TRUE) {
      grouping_columns <- c("RGN09CD", "Ethnic_Group", "Deprivation_Quintile", "Sex", "Age_Group", "Reg_Date")
      
      registered_deaths <- registered_deaths %>%
        left_join(utla_lkp, by = "UTLAApr19CD") %>%
        group_by(across(all_of(grouping_columns))) %>%
        summarise(deaths_total = sum(deaths_total), 
                  .groups = "drop")
      
      covid_deaths <- covid_deaths %>%
        left_join(utla_lkp, by = "UTLAApr19CD") %>%
        group_by(across(all_of(grouping_columns))) %>%
        summarise(deaths_total = sum(deaths_total), 
                  .groups = "drop")
      
      if (breakdown_by_ucod == TRUE) {
        ucod_deaths <- ucod_deaths %>%
          left_join(utla_lkp, by = "UTLAApr19CD") %>%
          group_by(across(all_of(grouping_columns))) %>%
          summarise(deaths_total = sum(deaths_total), 
                    .groups = "drop")
        
        
      }
      
      ethnicity_proportions <- calculate_ethnicity_proportions(data = registered_deaths,
                                                               ethnicity_field = Ethnic_Group,
                                                               region_field = RGN09CD,
                                                               age_field = Age_Group,
                                                               sex_field = Sex,
                                                               deaths_field = deaths_total,
                                                               by_date = TRUE)
      
      
    
  } 
  
  ### data checks ###
  registered_death_checks <- pre_processed_death_checks(deaths_data = registered_deaths, 
                                                        utla_lkp = utla_lkp, 
                                                        holidays = holidays, 
                                                        eth_dep = eth_dep, 
                                                        all_pod = all_pod)
  
  covid_death_checks <- pre_processed_death_checks(deaths_data = covid_deaths, 
                                                   utla_lkp = utla_lkp, 
                                                   holidays = holidays, 
                                                   eth_dep = eth_dep, 
                                                   all_pod = all_pod)
  
  if (breakdown_by_ucod == TRUE) {
    ucod_death_checks <- pre_processed_death_checks(deaths_data = ucod_deaths, 
                                                    utla_lkp = utla_lkp, 
                                                    holidays = holidays, 
                                                    eth_dep = eth_dep, 
                                                    all_pod = all_pod)
  }
  
  
  age_groups <- age_group_lkp(age_filter = age_filter,
                              type = age_group_type,
                              bespoke_age_groups = bespoke_age_groups) %>%
    pull(Age_Group) %>%
    unique()
  
  ethnic_groups <- c("Asian", "Black", "Mixed", "Other", "White")
  
  ### Preprocess registered deaths
  registered_deaths <- preprocessing_stages(data = registered_deaths, 
                                            eth_dep = eth_dep, 
                                            all_pod = all_pod, 
                                            ethnicity_proportions = ethnicity_proportions,
                                            deaths_field = deaths_total, 
                                            bank_holidays = holidays, 
                                            new_deaths_field_name = "registered_count")
  
  ### Preprocess covid deaths
  covid_deaths <- preprocessing_stages(data = covid_deaths, 
                                       eth_dep = eth_dep, 
                                       all_pod = all_pod, 
                                       ethnicity_proportions = ethnicity_proportions,
                                       deaths_field = deaths_total, 
                                       bank_holidays = holidays, 
                                       new_deaths_field_name = "deaths_total")
  
  if (breakdown_by_ucod == TRUE) {
    ucod_deaths <- preprocessing_stages(data = ucod_deaths, 
                                        eth_dep = eth_dep, 
                                        all_pod = all_pod, 
                                        ethnicity_proportions = ethnicity_proportions,
                                        deaths_field = deaths_total, 
                                        bank_holidays = holidays, 
                                        new_deaths_field_name = "ucod_disease")
  }
  
  
  ### data checks ###
  deaths_registered_post_checks <- post_processed_death_checks(registered_deaths,
                                                               holidays = holidays,
                                                               total_deaths = registered_death_checks$total_deaths,
                                                               eth_dep = eth_dep,
                                                               total_deaths_around_hols_weekends = registered_death_checks$total_deaths_around_hols_weekends,
                                                               deaths_field = registered_count)
  deaths_covid_post_checks <- post_processed_death_checks(covid_deaths,
                                                          holidays = holidays,
                                                          total_deaths = covid_death_checks$total_deaths,
                                                          eth_dep = eth_dep,
                                                          total_deaths_around_hols_weekends = covid_death_checks$total_deaths_around_hols_weekends,
                                                          deaths_field = deaths_total)
  
  if (breakdown_by_ucod == TRUE) {
    
    deaths_ucod_post_checks <- post_processed_death_checks(ucod_deaths,
                                                           holidays = holidays,
                                                           total_deaths = ucod_death_checks$total_deaths,
                                                           eth_dep = eth_dep,
                                                           total_deaths_around_hols_weekends = ucod_death_checks$total_deaths_around_hols_weekends,
                                                           deaths_field = ucod_disease)
  }
  
  
  if (!eth_dep == TRUE) {
    registered_deaths <- registered_deaths %>% 
      left_join(utla_lkp, by = "UTLAApr19CD")
    
    covid_deaths <- covid_deaths %>% 
      left_join(utla_lkp, by = "UTLAApr19CD")
    
    if (breakdown_by_ucod == TRUE) {
      ucod_deaths <- ucod_deaths %>% 
        left_join(utla_lkp, by = "UTLAApr19CD")
    }
  }
  
  registered_deaths <- registered_deaths %>% 
    group_by(across(all_of(grouping_fields))) %>%
    summarise(registered_count = sum(registered_count),
              .groups = "drop")
  
  covid_deaths <- covid_deaths %>% 
    group_by(across(all_of(grouping_fields))) %>%
    summarise(deaths_total = sum(deaths_total),
              .groups = "drop")
  
  if (breakdown_by_ucod == TRUE) {
    ucod_deaths <- ucod_deaths %>% 
      group_by(across(all_of(grouping_fields))) %>%
      summarise(ucod_disease = sum(ucod_disease),
                .groups = "drop")
  }
  
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
  
  if (breakdown_by_ucod == TRUE) {
    chart_data <- chart_data %>% 
      full_join(ucod_deaths, by = grouping_fields)
  }
  
  
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
  
  if (include_totals == TRUE) {
    if (!is.null(split_chart)) {
      if (visualisation_geography == "region") {
        additional_fields <- c("RGN09CD", "RGN09NM", "date")
      } else {
        additional_fields <- "date"
      }
      grouping_fields_totals <- c(split_chart, additional_fields)
      
      grouping_fields_missing <- setdiff(grouping_fields, grouping_fields_totals)
      if (length(grouping_fields_missing) == 0) {
        grouping_fields_missing <- split_chart
      } 
      
      # if (visualisation_geography == "region")
      #   grouping_fields_missing  <- grouping_fields_missing[!(grouping_fields_missing %in% "RGN09CD")]
      
      new_levels <- c(levels(chart_data[[grouping_fields_missing]]), "Total")  
      
      chart_data_totals <- chart_data %>%
        group_by(across(all_of(grouping_fields_totals))) %>%
        summarise(registered_count = sum(registered_count),
                  deaths_total = sum(deaths_total),
                  modelled_deaths = sum(modelled_deaths),
                  .groups = "drop") %>%
        mutate(!! rlang::sym(grouping_fields_missing) := "Total",
               modelled_deaths_zeros = ifelse(is.na(modelled_deaths), 0,
                                              modelled_deaths),
               area_name = "England") 
      
      if (visualisation_geography == "region") {
        chart_data_totals <- chart_data_totals %>% 
          mutate(area_name = RGN09NM)
      }
      
      chart_data <- chart_data %>%
        bind_rows(chart_data_totals) %>%
        mutate(!! rlang::sym(grouping_fields_missing) := factor(!! rlang::sym(grouping_fields_missing),
                                                                levels = new_levels))
    }
    else {
      warning ("split_chart needs to be populated in order for totals to be calculated.")
    }
  }
  

  export_powerbi_data(data = chart_data,
                      grouping_fields = grouping_fields,
                      model_filename = gsub("model_outputs/", "", gsub("\\.rds", "", model_filename)),
                      from_date = from_date,
                      end_date = end_date + 1,
                      age_filter = age_filter,
                      breakdown_by_ucod = breakdown_by_ucod,
                      report_type = report_type)
  
  return("complete")
}
