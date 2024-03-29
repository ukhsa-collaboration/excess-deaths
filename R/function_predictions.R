#' @description produces data from of predicted daily deaths, with confidence
#'   levels, by age, sex, and geography (based on the geography that was used in
#'   the modelling process). Ethnicity or deprivation are also included based on
#'   input arguments
#' @details where predictions have been made and stored on a central server,
#'   this function reads those csvs. If the prediction hasn't been stored, it
#'   creates them from the model object. Therefore this function may or may not
#'   need the model object to generate predicted deaths
#' @param model_filename file path to rds object to base predictions on
#' @param visualisation_geography string; the parent geography for the
#'   visualisation to be displayed. For example, to create regional charts as a
#'   subset in England, this argument would take "england". Possible values are
#'   "england", "region" or "utla" for all models except deprivation and
#'   ethnicity, where only "england" and "region" are possible values (based on
#'   the geography in the model). This parameter determines which geography the
#'   predictions are aggregated to along with facet_fields
#' @param from_date date; the start date for the predictions
#' @param to_date date; the final date for the predictions. This date acts as
#'   the final date that predictions are required for. If predictions do not
#'   exist up to that date, they are generated and combined with previously
#'   calculated predictions
#' @param directory string; the path to where the predictions files should be
#'   stored
#' @param facet_fields character vector up to length 2 (or NULL); use if
#'   predictions are to be made by grouped fields. Possible values are
#'   "Deprivation_Quintile", "Ethnic_Group", "Sex", "Age_Group", "RGN09CD",
#'   "UTLAApr19CD", "POD_out", "name_of_cause"
#' @age_group_type either "original" or "nomis" to control for the age groups
#'   used in the analysis
#' @inheritParams get_baseline_deaths

get_predictions <- function(model_filename, visualisation_geography = NULL,
                            from_date = as.Date("2020-03-01"), to_date = Sys.Date(), 
                            directory = Sys.getenv("PREDICTIONS_FILESHARE"),
                            eth_dep = FALSE, facet_fields = NULL,
                            age_filter = NULL, age_group_type = "original", 
                            bespoke_age_groups = NULL) {
  
  # check visualisation_geography inputs ------------------------------------
  geography_variable <- vis_geography_variable(visualisation_geography = visualisation_geography,
                                               eth_dep = eth_dep)  
  
  
  # check category variables ------------------------------------------------
  category_variables <- facet_variables(facet_fields = facet_fields,
                                        eth_dep = eth_dep)
  
  
  # create grouping fields vector -------------------------------------------
  grouping_fields <- c("date", geography_variable, category_variables)
  
  # the prediction files for name_of_cause and POD_out don't include a fields for those items
  # they are individual files for each COD/POD, identified by the file name
  # these fields need to be removed from the grouping variables
  remove_fields <- c("name_of_cause", "POD_out")
  grouping_fields <- grouping_fields[!(grouping_fields %in% remove_fields)]
  
  # change to_date to be the Friday after the last day of the year containing to_date
  last_day_of_year <- as.Date(paste0(year(to_date),
                                     "-12-31"))
  if (wday(last_day_of_year) == 7) { # if last date of year is Saturday
    days_until_following_fri <- 6  
  } else {
    days_until_following_fri <- 6 - wday(last_day_of_year)
  }
  to_date <- last_day_of_year + days_until_following_fri
  
  
  # create directory if it doesn't exist already
  dir.create(directory, showWarnings = FALSE)
  
  if (length(facet_fields) == 1) {
    if (facet_fields %in% c("name_of_cause", "POD_out")) facet_fields <- NULL #this forces the use of individual files
  }
  
  filename <- generate_file_name(model_filename = model_filename, 
                                 directory = directory, 
                                 visualisation_geography = visualisation_geography, 
                                 facet_fields = paste(facet_fields, collapse = "_"),
                                 age_filter = age_filter)
  if (file.exists(filename)) {
    predictions <- read.csv(filename)
    if (as.Date(max(predictions$date)) >= to_date) {
      return(predictions)
    } else {
      # if predictions csv already exists, predict new dates only
      predictions <- predictions %>% 
        mutate(date = as.Date(date))
      from_date <- max(predictions[["date"]]) + 1
      predictions_original <- predictions
    }
  }
  
  
  denominators <- get_denominators(start_year = year(from_date), 
                                   end_year = year(to_date), 
                                   eth_dep = eth_dep,
                                   age_filter = age_filter,
                                   age_group_type = age_group_type,
                                   bespoke_age_groups = bespoke_age_groups)
  
  
  # get holiday dates
  holidays <- holiday_dates(
    from_date = from_date,
    to_date = to_date
  )
  
  ## get utla lookup
  utla_lkp <- utla_lookup()
  
  ### data checks ###
  denominator_checks <- pre_processed_denominators_checks(denominators, 
                                                          start_year = year(from_date),
                                                          end_year = year(to_date),
                                                          utla_lkp = utla_lkp,
                                                          eth_dep = eth_dep,
                                                          age_filter = age_filter,
                                                          age_group_type = age_group_type,
                                                          bespoke_age_groups = bespoke_age_groups)
  
  # preprocess the denominators table
  if (eth_dep == TRUE) {
    denominators <- denominators %>%
      mutate(Ethnic_Group = factor(Ethnic_Group)) %>%
      mutate(Deprivation_Quintile = factor(Deprivation_Quintile))
    
  } else {
    denominators <- denominators %>%
      aggregate_Scillies_CoL(OfficialCode, denominator)
  }
  
  denominators <- denominators %>%
      sex_change_0_1(Sex) %>%
      mutate(Age_Group = factor(Age_Group))
  
  ### data checks ###
  denominator_post_checks <- post_processed_denominators_checks(denominators = denominators, 
                                                                total_denominators = denominator_checks$total_denominators, 
                                                                eth_dep = eth_dep)
  
  if (eth_dep == TRUE) {
    area_codes <- utla_lkp %>%
      pull(RGN09CD) %>%
      unique()
  } else {
    area_codes <- utla_lkp %>%
      filter(!(UTLAApr19CD %in% c("E06000053", "E09000001"))) %>%
      pull(UTLAApr19CD)
  }
  
  recent_dates <- build_recent_dates(area_codes = area_codes, 
                                     from_date = from_date, 
                                     to_date = to_date, 
                                     holidays = holidays, 
                                     denominators = denominators, 
                                     utla_lkp = utla_lkp,
                                     eth_dep = eth_dep,
                                     age_filter = age_filter,
                                     age_group_type = age_group_type,
                                     bespoke_age_groups = bespoke_age_groups)
  
  ##### BEGIN PREDICTING ######
  
  ########## England all ############
  model <- readRDS(model_filename)
  
  dispersion_parameter <- summary(model)$dispersion
  
  p2 <- predict(model, 
                newdata = recent_dates, 
                type = "response")
  
  predictions <- recent_dates %>%
    mutate(modelled_deaths = p2)
  
  if (eth_dep == FALSE) {
    predictions <- predictions %>%
      left_join(utla_lkp, by = "UTLAApr19CD")
  }
  
  predictions <- predictions %>%
    group_by(across(all_of(grouping_fields))) %>%
    summarise(modelled_deaths = sum(modelled_deaths), 
              .groups = "drop") %>%
    add_prediction_intervals(modelled_deaths, dispersion_parameter)
  
  if (file.exists(filename)) {
    # factors
    cols <- names(Filter(is.factor, predictions))
    predictions_original <- predictions_original %>% 
      mutate(across(all_of(cols), factor))
    
    # dates
    cols <- names(Filter(is.Date, predictions))
    predictions_original <- predictions_original %>% 
      mutate(across(all_of(cols), as.Date))
    
    # integers
    cols <- names(Filter(is.integer, predictions))
    predictions_original <- predictions_original %>% 
      mutate(across(all_of(cols), as.integer))
    
    
    predictions <- bind_rows(predictions_original,
                             predictions)
  }
  
  write.csv(predictions,
            file = filename,
            row.names = FALSE)
  return(predictions)
  
}