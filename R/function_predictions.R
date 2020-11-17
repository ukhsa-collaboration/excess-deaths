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
#' @param to_date date; the final date for the predictions
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
                            ethnicity = FALSE, deprivation = NULL, facet_fields = NULL,
                            age_filter = NULL, age_group_type = "original") {

  
  # check visualisation_geography inputs ------------------------------------
  geography_variable <- vis_geography_variable(visualisation_geography = visualisation_geography,
                                               ethnicity = ethnicity,
                                               deprivation = deprivation)  
  
  
  # check category variables ------------------------------------------------
  category_variables <- facet_variables(facet_fields = facet_fields,
                                        ethnicity = ethnicity,
                                        deprivation = deprivation)
  
  
  # create grouping fields vector -------------------------------------------
  grouping_fields <- c("date", geography_variable, category_variables)
  
  # the prediction files for name_of_cause and POD_out don't include a fields for those items
  # they are individual files for each COD/POD, identified by the file name
  # these fields need to be removed from the grouping variables
  remove_fields <- c("name_of_cause", "POD_out")
  grouping_fields <- grouping_fields[!(grouping_fields %in% remove_fields)]
  
  
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
    predictions <- read.csv(filename, stringsAsFactors = FALSE)
    if (as.Date(max(predictions$date)) > Sys.Date()) return(predictions)
  }
  
  
  denominators <- get_denominators(start_year = year(from_date), 
                                   end_year = year(to_date), 
                                   ethnicity = ethnicity, 
                                   deprivation = deprivation,
                                   age_filter = age_filter)
  
  
  # get holiday dates
  holidays <- timeDate::holidayLONDON(year(from_date):year(to_date)) #get vector of bank holidays
  holidays <- as.Date(holidays)
  holidays <- replace(holidays, holidays == as.Date("2020-05-04"), as.Date("2020-05-08"))
  
  ## get utla lookup
  utla_lkp <- utla_lookup()
  
  ### data checks ###
  denominator_checks <- pre_processed_denominators_checks(denominators, 
                                                          start_year = year(from_date),
                                                          end_year = year(to_date),
                                                          utla_lkp = utla_lkp,
                                                          ethnicity = ethnicity, 
                                                          deprivation = deprivation,
                                                          age_filter = age_filter)
  
  # preprocess the denominators table
  if (ethnicity == TRUE) {
    denominators <- denominators %>%
      mutate(Ethnic_Group = factor(Ethnic_Group))
    
  } 
  
  if (deprivation == TRUE) {
    denominators <- denominators %>%
      mutate(Deprivation_Quintile = factor(Deprivation_Quintile))
    
  }
  
  if (deprivation == FALSE & ethnicity == FALSE) {
    denominators <- denominators %>%
      aggregate_Scillies_CoL(OfficialCode, denominator)
  }
  
  denominators <- denominators %>%
      sex_change_0_1(Sex) %>%
      mutate(Age_Group = factor(Age_Group))
  
  ### data checks ###
  denominator_post_checks <- post_processed_denominators_checks(denominators = denominators, 
                                                                total_denominators = denominator_checks$total_denominators, 
                                                                ethnicity = ethnicity, 
                                                                deprivation = deprivation)
  
  if (ethnicity == TRUE | deprivation == TRUE) {
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
                                     ethnicity = ethnicity, 
                                     deprivation = deprivation,
                                     age_filter = age_filter,
                                     age_group_type = age_group_type)
  
  ##### BEGIN PREDICTING ######
  
  ########## England all ############
  model <- readRDS(model_filename)
  
  dispersion_parameter <- summary(model)$dispersion
  
  p2 <- predict(model, 
                newdata = recent_dates, 
                type = "response")
  
  predictions <- recent_dates %>%
    mutate(modelled_deaths = p2)
  
  if (ethnicity != TRUE & deprivation != TRUE) {
    predictions <- predictions %>%
      left_join(utla_lkp, by = "UTLAApr19CD")
  }
  
  predictions <- predictions %>%
    group_by_at(grouping_fields) %>%
    summarise(modelled_deaths = sum(modelled_deaths), 
              .groups = "keep") %>%
    ungroup() %>%
    add_prediction_intervals(modelled_deaths, dispersion_parameter)
  
  write.csv(predictions,
            file = filename,
            row.names = FALSE)
  return(predictions)
  
}