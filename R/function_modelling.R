#' @description creates baseline models and saves them as rds file; model
#'   created is based on whether deprivation, ethnicity or neither are included
#' @inheritParams get_baseline_deaths
#' @param denominators data frame or tibble of denominators with the structure
#'   of the output of the get_denominators() function
#' @param model_filename string; name of file to be saved as output (extension
#'   is not required)
#' @param age_filter numeric vector (length 2); an upper and lower threshold to
#'   truncate the age range for analysis. Can use NA for either value if ages up
#'   to the limit should be considered (ie, c(50, NA) will consider ages between
#'   50 and the maximum age in the dataset)
#' @return string presenting the location of the model file generated

create_baseline <- function(denominators = NULL,
                            ucod = NULL, ucods = NULL, cod = NULL,
                            pod = NULL, eth_dep = FALSE,
                            model_filename, 
                            age_filter = NULL,
                            age_group_type = "original",
                            bespoke_age_groups = NULL) {
 
  # create directory if it doesn't exist already
  dir.create("model_outputs", showWarnings = FALSE)
  
   model_filename <- paste0("model_outputs/", model_filename, ".rds")
  
  
  ###-------------### BRING THE DATA IN TO THE ENVIRONMENT ###------------###
  baseline_data <- get_baseline_deaths(ucod = ucod, 
                                       ucods = ucods,
                                       cod = cod,
                                       pod = pod, 
                                       eth_dep = eth_dep, 
                                       age_filter = age_filter,
                                       age_group_type = age_group_type,
                                       bespoke_age_groups = bespoke_age_groups)
  
  if (is.null(denominators)) {
    denominators <- get_denominators(start_year = min(year(baseline_data$Reg_Date)),
                                     end_year = max(year(baseline_data$Reg_Date)),
                                     eth_dep = eth_dep,
                                     age_filter = age_filter,
                                     bespoke_age_groups = bespoke_age_groups)
  }
  
  utla_lkp <- utla_lookup()
  
  from_date <- as.Date("2015-01-01")
  to_date <- as.Date("2019-12-31")
  
  # get holiday dates
  holidays <- holiday_dates(
    from_date = from_date,
    to_date = to_date
  )
  
  ### data checks ###
  death_checks <- pre_processed_death_checks(baseline_data, 
                                             utla_lkp, 
                                             holidays,
                                             eth_dep = eth_dep)
  
  denominator_checks <- pre_processed_denominators_checks(denominators, 
                                                          start_year = min(year(baseline_data$Reg_Date)),
                                                          end_year = max(year(baseline_data$Reg_Date)),
                                                          utla_lkp = utla_lkp,
                                                          eth_dep = eth_dep,
                                                          age_filter = age_filter,
                                                          age_group_type = age_group_type,
                                                          bespoke_age_groups = bespoke_age_groups)
  
  ###-------------### DATA PREPARATION ###------------###
  
  # preprocess the denominators table
  if (eth_dep == TRUE) {
    denominators <- denominators %>%
        mutate(Ethnic_Group = factor(Ethnic_Group),
               Deprivation_Quintile = factor(Deprivation_Quintile, 
                                             levels = as.character(1:5))) %>% 
      rename(RGN09CD = OfficialCode)
  } else if (eth_dep == FALSE) {
    denominators <- denominators %>% 
      aggregate_Scillies_CoL(OfficialCode, denominator) %>% 
      rename(UTLAApr19CD = OfficialCode)
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
                                           levels = as.character(1:5)))
    
  } else {
    baseline_data <- baseline_data %>%
      aggregate_Scillies_CoL(UTLAApr19CD, deaths_total) %>%
      arrange(UTLAApr19CD, Sex, Age_Group, Reg_Date) %>%
      preprocess_deaths(date_field = Reg_Date, 
                        death_field = deaths_total, 
                        sex_field = Sex, 
                        age_group_field = Age_Group, 
                        holidays = holidays)
    
  }
  
  ### data checks ###
  deaths_post_checks <- post_processed_death_checks(baseline_data, 
                                                    holidays = holidays,
                                                    total_deaths = death_checks$total_deaths,
                                                    total_deaths_around_hols_weekends = death_checks$total_deaths_around_hols_weekends,
                                                    eth_dep = eth_dep,
                                                    deaths_field = deaths_total)
  
  
  ###------------### Add predictor variables to baseline data ###------------###
  
  # aggregate denominator data to weekly
  denominators <- weekly_denominators(
    denominators = denominators,
    from_date = from_date,
    to_date = to_date
  )
  
  # create date dependent variables
  hol_vars <- weekly_holiday_variables(
    from_date = from_date,
    to_date = to_date,
    holidays = holidays
  )
  
  trend_var <- weekly_trend_variable(
    from_date = from_date,
    to_date = to_date
  )
  
  seasonal_vars <- weekly_seasonal_variables(
    from_date = from_date,
    to_date = to_date
  )
  
  date_dependent_variables <- hol_vars %>% 
    left_join(seasonal_vars, by = "date") %>% 
    left_join(trend_var, by = "date")
  
  baseline_data <- baseline_data %>%
    # add the aggregated denominator for each subgroup
    left_join(denominators, by = intersect(names(.), 
                                           names(denominators))) %>% 
    left_join(date_dependent_variables,
              by = "date")
  
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
                           wk_sat_to_mon_xmas + wk_post_sat_to_mon_xmas + wk2_post_sat_to_mon_xmas"
    
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
