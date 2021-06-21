#' @description create visualisations for displaying excess mortality
#' @inheritParams get_predictions
#' @inheritParams get_baseline_deaths
#' @param eth_dep logical; whether ethnicity and deprivation were included in
#'   the model the visualisation is based on
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
#' @param to_date date; date passed to get_predictions. This date acts as the
#'   final date that predictions are required for. If predictions do not exist
#'   up to that date, they are generated and combined with previously calculated
#'   predictions
#' @param show_inset logical; display explanatory inset in weekly chart
#'   visualisation
#' @param split_chart string; if facet_fields is not null, field name that
#'   determines how the facets are tabbed. Options are the same as those for
#'   facet_fields. If left blank, then charts are displayed without tabs
#' @param subtitle string; text to display as subtitle in visualisation
#' @param axis_title string; text to display on the y axis of the cumulative
#'   charts and at the top of the left hand column of the table
#' @param include_totals logical; whether to include a group for totals in the
#'   age-sex visualisations
#' @param age_filter numeric vector (length 2); an upper and lower threshold to
#'   truncate the age range for analysis. Can use NA for either value if ages up
#'   to the limit should be considered (ie, c(50, NA) will consider ages between
#'   50 and the maximum age in the dataset)
#' @param age_group_type either "nomis" or "original"
#' @param display_cis logical; whether to display confidence intervals and
#'   display the bars in the ratio charts
#' @param stop_before_visualisation logical; whether to stop the code before hte
#'   visualisation is created. This is useful for when producing the powerbi
#'   data files is the important objective

generate_visualisations <- function(model_filename, visualisation_geography = "england",
                                    from_date = as.Date("2020-03-20"), to_date = ceiling_date(Sys.Date(), unit = "years") - 1, 
                                    directory = Sys.getenv("PREDICTIONS_FILESHARE"),
                                    ucod = NULL, btw_ucod = NULL, ucods = NULL, cod = NULL,
                                    pod = NULL, all_pod = FALSE, covid_only = FALSE, 
                                    cause_name = "all cause",
                                    caption = "", include_covid_ribbon = FALSE, eth_dep = FALSE, facet_fields = NULL,
                                    end_date = Sys.Date(), show_inset = FALSE, split_chart = NULL,
                                    subtitle = "", axis_title = "", include_totals = FALSE, age_filter = NULL,
                                    age_group_type = "original", display_cis = FALSE, 
                                    stop_before_visualisation = FALSE) {
  

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
      any(!is.null(ucod), !is.null(btw_ucod), !is.null(ucods))) breakdown_by_ucod <- TRUE
  
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
                        eth_dep = eth_dep, 
                        facet_fields = facet_fields,
                        age_filter = age_filter,
                        age_group_type = age_group_type)
  if (all_pod == TRUE) {
    predictions <- map_df(predictions, ~as.data.frame(.x), .id = "POD_out")
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
                                           age_filter = age_filter)
    
    # deaths with underlying cause
    ucod_deaths <- get_recent_deaths(ucod = ucod, 
                                     btw_ucod = btw_ucod, 
                                     ucods = ucods, 
                                     pod = pod,
                                     covid_only = FALSE, 
                                     end_date = end_date,
                                     eth_dep = eth_dep,
                                     all_pod = all_pod,
                                     age_filter = age_filter)
  } else {
    registered_deaths <- get_recent_deaths(ucod = ucod, 
                                           btw_ucod = btw_ucod, 
                                           ucods = ucods, 
                                           cod = cod, 
                                           pod = pod,
                                           covid_only = FALSE, 
                                           end_date = end_date,
                                           eth_dep = eth_dep,
                                           all_pod = all_pod,
                                           age_filter = age_filter)
  }
  
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
                                    end_date = end_date,
                                    eth_dep = eth_dep,
                                    all_pod = all_pod,
                                    age_filter = age_filter)
  
  
  # remove first day of series if a bank holiday or weekend
  registered_deaths <- registered_deaths %>%
    adjust_first_date(Reg_Date, holidays) %>%
    adjust_last_date(Reg_Date, holidays)

  covid_deaths <- covid_deaths %>%
    adjust_first_date(Reg_Date, holidays) %>%
    adjust_last_date(Reg_Date, holidays)
  
  if (breakdown_by_ucod == TRUE) {
    ucod_deaths <- ucod_deaths %>%
      adjust_first_date(Reg_Date, holidays) %>%
      adjust_last_date(Reg_Date, holidays)
  }
  
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
                                                        deaths_reallocated = TRUE,
                                                        all_pod = all_pod)
  
  covid_death_checks <- pre_processed_death_checks(deaths_data = covid_deaths, 
                                                   utla_lkp = utla_lkp, 
                                                   holidays = holidays, 
                                                   eth_dep = eth_dep, 
                                                   deaths_reallocated = TRUE,
                                                   all_pod = all_pod)
  
  if (breakdown_by_ucod == TRUE) {
    ucod_death_checks <- pre_processed_death_checks(deaths_data = ucod_deaths, 
                                                    utla_lkp = utla_lkp, 
                                                    holidays = holidays, 
                                                    eth_dep = eth_dep, 
                                                    deaths_reallocated = TRUE,
                                                    all_pod = all_pod)
  }
  
  
  age_groups <- age_group_lkp(type = age_group_type) %>%
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
                                                               deaths_reallocated = TRUE,
                                                               days_reallocated = FALSE,
                                                               deaths_field = registered_count)
  deaths_covid_post_checks <- post_processed_death_checks(covid_deaths,
                                                          holidays = holidays,
                                                          total_deaths = covid_death_checks$total_deaths,
                                                          eth_dep = eth_dep,
                                                          total_deaths_around_hols_weekends = covid_death_checks$total_deaths_around_hols_weekends,
                                                          deaths_reallocated = TRUE,
                                                          days_reallocated = FALSE,
                                                          deaths_field = deaths_total)
  
  if (breakdown_by_ucod == TRUE) {
    
    deaths_ucod_post_checks <- post_processed_death_checks(ucod_deaths,
                                                           holidays = holidays,
                                                           total_deaths = ucod_death_checks$total_deaths,
                                                           eth_dep = eth_dep,
                                                           total_deaths_around_hols_weekends = ucod_death_checks$total_deaths_around_hols_weekends,
                                                           deaths_reallocated = TRUE,
                                                           days_reallocated = FALSE,
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
  
  # convert all daily death tables to weekly deaths
  registered_deaths <- registered_deaths %>% 
    filter(date >= from_date) %>%
    convert_daily_to_weekly(fields_for_grouping = grouping_fields,
                            date_field = date,
                            aggregate_field = registered_count)
  
  covid_deaths <- covid_deaths %>% 
    filter(date >= from_date) %>%
    convert_daily_to_weekly(fields_for_grouping = grouping_fields,
                            date_field = date,
                            aggregate_field = deaths_total)
  
  if (breakdown_by_ucod == TRUE) {
    ucod_deaths <- ucod_deaths %>% 
      filter(date >= from_date) %>%
      convert_daily_to_weekly(fields_for_grouping = grouping_fields,
                              date_field = date,
                              aggregate_field = ucod_disease)
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
  
  #### BEGIN PLOTTING ####
  
  excessdeaths_chart <- list()
  weekly_chart_simple <- list()
  final_cumulative <- list()
  
  export_powerbi_data(data = chart_data, 
                      grouping_fields = grouping_fields,
                      model_filename = gsub("model_outputs/", "", gsub("\\.rds", "", model_filename)), 
                      from_date = from_date,
                      end_date = end_date + 1,
                      age_filter = age_filter,
                      breakdown_by_ucod = breakdown_by_ucod)
  
  if (stop_before_visualisation == TRUE) return("complete")
  
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
        
        if (all_pod == TRUE) {
          title_part <- gsub("^home$", "own home", tolower(splitting_items))
          cause_name <- setNames(title_part,
                                 nm = splitting_items)
        } else if (split_chart == "Deprivation_Quintile" & facet_fields == "Deprivation_Quintile") {
          cause_name <- setNames(splitting_items,
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
                                                     dispersion_parameter = dispersion_parameter,
                                                     display_cis = display_cis)
      
      
    } else if (length(facet_fields) == 2) {
      splitting_items <- chart_data_filtered %>%
        pull(split_chart) %>%
        unique()
      
      facet_var <- setdiff(facet_fields, split_chart)
      
      cause_name <- setNames(tolower(splitting_items),
                             nm = splitting_items)
      
      sub_split <- setdiff(facet_fields, split_chart)
      
      for (i in splitting_items) {
        for (j in levels(chart_data_filtered[[sub_split]])) {
          if (sub_split != "Ethnic_Group") {
            subtitle_string <- tolower(j)
          } else {
            subtitle_string <- j
          }
          weekly_chart_simple[[area]][[i]][[j]] <- weekly_deaths_simple(death_data = chart_data_filtered %>% filter(!! sym(split_chart) == i,
                                                                                                                    !! sym(sub_split) == j),
                                                                        area_name = area,
                                                                        cause_name = paste0(cause_name[i], " (", subtitle_string, ")"),
                                                                        date_field = date,
                                                                        model_field = modelled_deaths_zeros,
                                                                        deaths_field = registered_count,
                                                                        covid_field = deaths_total,
                                                                        start_date = from_date,
                                                                        end_date = end_date,
                                                                        subtitle = subtitle,
                                                                        caption = caption)
        }
      }
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
                                              dispersion_parameter = dispersion_parameter,
                                              display_cis = display_cis))
      names(final_cumulative[[area]]) <- splitting_items
       
    }
    
  }
  
  return(list(weekly_simple = weekly_chart_simple,
              cumulative = excessdeaths_chart,
              cumulative_compare = final_cumulative))
 
}
