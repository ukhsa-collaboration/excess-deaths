#' @description creates baseline models and saves them as rds file; model
#'   created is based on whether deprivation, ethnicity or neither are included
#' @inheritParams get_baseline_deaths
#' @param denominators data frame or tibble of denominators with the structure
#'   of the output of the get_denominators() function
#' @param model_filename string; name of file to be saved as output (extension
#'   is not required)
#' @param include_date_extension logical; whether the date is automatically
#'   appended to the file name
#' @param disease_name string; name of the disease for the model. This string is
#'   used to filter the comparability ratio Excel file, so the string must match
#'   the spelling used in that file
#' @param include_ethnicity_uplift logical; whether ethnicity baseline deaths
#'   should be uplifted to the same totals as the annual deaths file. Ethnic
#'   group for individuals is obtains from HES records. If an individual hasn't
#'   been to hospital, they won't be in the ethnicity deaths file
#' @param age_filter numeric vector (length 2); an upper and lower threshold to
#'   truncate the age range for analysis. Can use NA for either value if ages up
#'   to the limit should be considered (ie, c(50, NA) will consider ages between
#'   50 and the maximum age in the dataset)
#' @return string presenting the location of the model file generated

create_baseline <- function(denominators = NULL,
                            ucod = NULL, btw_ucod = NULL, ucods = NULL, cod = NULL,
                            pod = NULL, eth_dep = FALSE,
                            model_filename, include_date_extension = FALSE,
                            disease_name = NULL, include_ethnicity_uplift = FALSE, age_filter = NULL,
                            age_group_type = "original") {
 
  # create directory if it doesn't exist already
  dir.create("model_outputs", showWarnings = FALSE)
  if (include_date_extension == TRUE) {
    model_filename <- paste0("model_outputs/", model_filename, gsub("[[:punct:]]", "", Sys.Date()), ".rds")
  } else {
    model_filename <- paste0("model_outputs/", model_filename, ".rds")
  }
  
  ###-------------### BRING THE DATA IN TO THE ENVIRONMENT ###------------###
  baseline_data <- get_baseline_deaths(ucod = ucod, 
                                       btw_ucod = btw_ucod, 
                                       ucods = ucods,
                                       cod = cod,
                                       pod = pod, 
                                       eth_dep = eth_dep, 
                                       include_ethnicity_uplift = include_ethnicity_uplift,
                                       age_filter = age_filter,
                                       age_group_type = age_group_type)
  
  if (is.null(denominators)) {
    denominators <- get_denominators(start_year = min(year(baseline_data$Reg_Date)),
                                     end_year = max(year(baseline_data$Reg_Date)),
                                     eth_dep = eth_dep,
                                     age_filter = age_filter)
  }
  
  utla_lkp <- utla_lookup()
  
  # get holiday dates
  holidays <- timeDate::holidayLONDON(year(min(baseline_data$Reg_Date)):year(Sys.Date())) #get vector of bank holidays
  holidays <- as.Date(holidays)
  holidays <- replace(holidays, holidays == as.Date("2020-05-04"), as.Date("2020-05-08"))
  
  # remove first day of series if a bank holiday or weekend
  baseline_data <- baseline_data %>%
    adjust_first_date(Reg_Date, holidays)
  
  
  ### data checks ###
  death_checks <- pre_processed_death_checks(baseline_data, 
                                             utla_lkp, 
                                             holidays,
                                             eth_dep = eth_dep,
                                             deaths_reallocated = TRUE)
  
  denominator_checks <- pre_processed_denominators_checks(denominators, 
                                                          start_year = min(year(baseline_data$Reg_Date)),
                                                          end_year = max(year(baseline_data$Reg_Date)),
                                                          utla_lkp = utla_lkp,
                                                          eth_dep = eth_dep,
                                                          age_filter = age_filter)
  
  ###-------------### DATA PREPARATION ###------------###
  
  # preprocess the denominators table
  if (eth_dep == TRUE) {
    denominators <- denominators %>%
        mutate(Ethnic_Group = factor(Ethnic_Group),
               Deprivation_Quintile = factor(Deprivation_Quintile, 
                                             levels = as.character(1:5)))
  }
  
  denominators <- denominators %>%
    sex_change_0_1(Sex) %>%
    mutate(Age_Group = factor(Age_Group))
  
  ### data checks ###
  denominator_post_checks <- post_processed_denominators_checks(denominators, 
                                                                denominator_checks$total_denominators,
                                                                eth_dep = eth_dep)
  
  # do preprocessing steps on baseline data
  if (eth_dep == TRUE) {
    baseline_data <- baseline_data %>%
      arrange(RGN09CD, Ethnic_Group, Deprivation_Quintile, Sex, Age_Group, Reg_Date) %>%
      preprocess_deaths(date_field = Reg_Date, 
                        death_field = deaths_total, 
                        sex_field = Sex, 
                        age_group_field = Age_Group, 
                        holidays = holidays) %>%
      mutate(Ethnic_Group = factor(Ethnic_Group),
             Deprivation_Quintile = factor(Deprivation_Quintile, 
                                           levels = as.character(1:5))) %>%
      left_join(denominators, by = c("RGN09CD" = "OfficialCode", "Ethnic_Group", "Deprivation_Quintile", "Sex", "Age_Group", "month")) %>%
      dplyr::select(-month)
  } else {
    baseline_data <- baseline_data %>%
      aggregate_Scillies_CoL(UTLAApr19CD, deaths_total) %>%
      arrange(UTLAApr19CD, Sex, Age_Group, Reg_Date) %>%
      preprocess_deaths(date_field = Reg_Date, 
                        death_field = deaths_total, 
                        sex_field = Sex, 
                        age_group_field = Age_Group, 
                        holidays = holidays) %>%
      left_join(denominators, by = c("UTLAApr19CD" = "OfficialCode", "Sex", "Age_Group", "month")) %>%
      dplyr::select(-month)
    
    
  }
  
  ### data checks ###
  deaths_post_checks <- post_processed_death_checks(baseline_data, 
                                                    holidays = holidays,
                                                    total_deaths = death_checks$total_deaths,
                                                    total_deaths_around_hols_weekends = death_checks$total_deaths_around_hols_weekends,
                                                    eth_dep = eth_dep,
                                                    deaths_reallocated = TRUE,
                                                    deaths_field = deaths_total)
  
  # calculate easter Fridays
  easter_fridays <- calc_easter_fridays(holidays)
  non_easter_non_xmas_hols <- calc_non_easter_non_xmas_hols(holidays, easter_fridays)
  ###------------### Add predictor variables to baseline data ###------------###
  # browser()
  baseline_data <- baseline_data %>%
    add_easter_binary_variables(date, easter_fridays) %>%
    add_bh_binary_variables(date_field = date, day_field = day, non_easter_non_xmas_hols) %>%
    add_day_weighting(date_field = date) %>% 
    pivot_wider(names_from = month,
                names_prefix = "month",
                values_from = month_val,
                values_fill = list(month_val = 0)) %>%
    arrange(date) %>% 
    mutate(day1 = if_else(day == "2", 1L, 0L),
           day2 = if_else(day == "3", 1L, 0L),
           day3 = if_else(day == "4", 1L, 0L),
           day4 = if_else(day == "5", 1L, 0L),
           day5 = if_else(day == "6", 1L, 0L)) %>% 
    dplyr::select(-day) %>%
    ungroup()
  
  
  baseline_data <- convert_to_weekly_for_modelling(data = baseline_data,
                                                   from_date = min(baseline_data$date),
                                                   to_date = max(baseline_data$date))
  modelling_variables <- "deaths_total ~ offset(log(denominator)) +
                           years_from_20161231 +
                           month1:Age_Group + month2:Age_Group + 
                           month3:Age_Group + month4:Age_Group +
                           month5:Age_Group + month6:Age_Group + 
                           month7:Age_Group + month8:Age_Group +
                           month9:Age_Group + month10:Age_Group + 
                           month11:Age_Group + month12:Age_Group +
                           easter_pre + easter_post_1 + easter_post_2 +
                           wk_nearest_BH + wk_next_nearest_BH +
                           wk_fri_xmas + wk_post_fri_xmas + wk2_post_fri_xmas"
    
  cat("building model...")
  start_time <- Sys.time()
  if (eth_dep == TRUE) {
    modelling_variables <- paste(modelling_variables,
                                 "+ Ethnic_Group:Deprivation_Quintile +
                                   Ethnic_Group:Age_Group +
                                   Deprivation_Quintile:Age_Group +
                                   years_from_20161231:Deprivation_Quintile +
                                   years_from_20161231:Age_Group +
                                   Sex:Age_Group +
                                   RGN09CD")
    
  } else  {
    modelling_variables <- paste(modelling_variables,
                                 "+ Sex:Age_Group +                 
                                   UTLAApr19CD")
    
  }
    
  model <- glm(modelling_variables, 
               family = quasipoisson,
               data = baseline_data)
  cat(paste0("finished...", capture.output(Sys.time() - start_time)))
  cat("\nsaving model")
  saveRDS(model,
          model_filename)
  
  # save dispersion parameter
  dispersion_parameter <- summary(model)$dispersion
  
  disp_par_filename <- filename_model_to_dispersion(model_filename)
  writeLines(as.character(dispersion_parameter), disp_par_filename)
  
  return(model_filename)
  
}
