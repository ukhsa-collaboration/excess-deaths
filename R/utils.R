#' @description aggregates data for IoS with Cornwall and CoL with Hackney
aggregate_Scillies_CoL <- function(data, area_code, agg_field) {
  IoS <- "E06000053"
  Corn <- "E06000052"
  
  CoL <- "E09000001"
  Hack <- "E09000012"
  data <- data %>%
    mutate({{ area_code }} := case_when({{ area_code }} == IoS ~ Corn,
                                                     {{ area_code }} == CoL ~ Hack,
                                                     TRUE ~ {{ area_code}})) %>%
    group_by(across(!c({{ agg_field }}))) %>%
    summarise({{ agg_field }} := sum({{ agg_field }}),
              .groups = "drop")
  return(data)
}

#' @description returns a vector of easter fridays when given a vector of holiday dates
calc_easter_fridays <- function(dates, include_christmas_friday = TRUE) {
  easter_fridays <- tibble(holiday_date = dates) %>%
    mutate(Day_name = wday(holiday_date,
                           label = TRUE,
                           abbr = FALSE),
           easter_friday = holiday_date %in% (dates - 3),
           easter_monday = holiday_date %in% (dates + 3),
           month = month(holiday_date),
           easter_weekend = case_when(
             easter_monday | easter_friday ~ TRUE,
             TRUE ~ FALSE
           ))
  
  if (include_christmas_friday == TRUE) {
    easter_fridays <- easter_fridays %>%
      filter(easter_friday)
  } else {
    easter_fridays <- easter_fridays %>%
      filter(easter_friday,
             month != 12)
  }
  easter_fridays <- easter_fridays %>%
    arrange(holiday_date) %>%
    pull(holiday_date)
  return(easter_fridays)
}

#' @description returns a vector of dates that aren't easter or xmas related
calc_non_easter_non_xmas_hols <- function(dates, easter_fridays) {
  dates <- dates[!(dates %in% c(easter_fridays, easter_fridays + 3))]
  dates <- dates[month(dates) != 12]
  return(dates)
}

#' @description adds binary variables for days around easter holidays
add_easter_binary_variables <- function(data, date_field, easter_fridays) {
  data  <- data %>% 
    mutate(WedpreE = case_when(
             {{ date_field }} %in% (easter_fridays - 2) ~ 1L,
             TRUE ~ 0L),
           ThurpreE = case_when(
             {{ date_field }} %in% (easter_fridays - 1) ~ 1L,
             TRUE ~ 0L),
           TuespostE = case_when(
             {{ date_field }} %in% (easter_fridays + 4) ~ 1L,
             TRUE ~ 0L),
           WedpostE = case_when(
             {{ date_field }} %in% (easter_fridays + 5) ~ 1L,
             TRUE ~ 0L),
           ThurpostE = case_when(
             {{ date_field }} %in% (easter_fridays + 6) ~ 1L,
             TRUE ~ 0L),
           FripostE = case_when(
             {{ date_field }} %in% (easter_fridays + 7) ~ 1L,
             TRUE ~ 0L),
           MonpostE1 = case_when(
             {{ date_field }} %in% (easter_fridays + 10) ~ 1L,
             TRUE ~ 0L),
           TuespostE1 = case_when(
             {{ date_field }} %in% (easter_fridays + 11) ~ 1L,
             TRUE ~ 0L))
  return(data)
}

#' @description adds binary variables for days around bank holidays that aren't easter or xmas
add_bh_binary_variables <- function(data, date_field, day_field, holidays) {
  easter_fridays <- calc_easter_fridays(holidays)
  
  non_easter_holidays <- holidays[!(holidays %in% c(easter_fridays, (easter_fridays + 3)))]
  
  friday_bhols <- non_easter_holidays[wday(non_easter_holidays) == 6 & 
                                        month(non_easter_holidays) != 12]
  monday_bhols <- non_easter_holidays[wday(non_easter_holidays) == 2 & 
                                        month(non_easter_holidays) != 12]
  other_bhols <- non_easter_holidays[!(non_easter_holidays %in% c(friday_bhols, monday_bhols))]
  
  data  <- data %>% 
    mutate(BH_nearest_WD = case_when(({{ day_field }} == 5 & 
                                 {{ date_field }} %in% (friday_bhols - 1)) |
                                   ({{ day_field }} == 3 & 
                                      {{ date_field }} %in% (monday_bhols + 1)) ~ 1L,
                                 TRUE ~ 0L),
           BH_next_nearest_WD = case_when(({{ day_field }} == 6 & 
                                             {{ date_field }} %in% (monday_bhols - 3)) |
                                            ({{ day_field }} == 2 & 
                                               {{ date_field }} %in% (friday_bhols + 3)) ~ 1L,
                                          TRUE ~ 0L))
  return(data)
}

#' @description adds a weighting score between 0 and 1 to indicate how much of a weight the month that day falls into should influence the model
add_day_weighting <- function(data, date_field) {
  month_earlier <- data %>%
    mutate(dom = day({{ date_field }}),
           month = case_when(
             dom < 17 ~ case_when(
               month({{ date_field }}) == 1 ~ 12L, 
               TRUE ~ as.integer(month({{ date_field }}) - 1)),
             TRUE ~ as.integer(month({{ date_field }}))),
           month_val = case_when(
             dom < 17 ~ (16 - dom) / 30,
             TRUE ~ (46 - dom) / 30))
  
  month_later <- data %>%
    mutate(dom = day({{ date_field }}),
           month = case_when(
             dom > 16 ~ case_when(
               month({{ date_field }}) == 12 ~ 1L, 
               TRUE ~ as.integer(month({{ date_field }}) + 1)),
             TRUE ~ as.integer(month({{ date_field }}))),
           month_val = case_when(
             dom > 16 ~ (dom - 16) / 30,
             TRUE ~ (14 + dom) / 30))
  
  data <- bind_rows(month_earlier, month_later) %>%
    dplyr::select(-dom) %>%
    arrange(month)
  
  return(data)
}

#' @description moves all agg_field values recorded on a saturday to the friday
#'   prior to the weekend, and the sunday to the monday following the weekend
weekends_to_nearest_work_day <- function(data, date_field, agg_field) {
  
  
  weekend_data <- data %>%
    mutate(sunday = wday({{ date_field }}) %in% c(7),
           sunday_mark = case_when(sunday == TRUE | 
                                     lag(sunday) == TRUE |
                                     lag(sunday, n = 2) == TRUE ~ TRUE,
                                   TRUE ~ FALSE))
  
  remove_vars_from_grouping <- c(rlang::as_name(enquo(agg_field)), 
                                 rlang::as_name(enquo(date_field)), 
                                 "sunday_mark", "sunday")
    
  monday_data <- weekend_data %>%
    mutate(group_id = cumsum(sunday)) %>%
    filter(sunday_mark == TRUE) %>%
    group_by(across(-all_of(remove_vars_from_grouping))) %>%
    summarise({{ date_field }} := max({{ date_field }}),
              aggregated_sunday_data = sum({{ agg_field }}),
              .groups = "drop") %>%
    dplyr::select(-group_id)
  
  data <- data %>%
    left_join(monday_data, 
              by = intersect(names(.), names(monday_data))) %>%
    mutate({{ agg_field }} := case_when(wday({{ date_field }}) %in% c(1, 7) ~ 0,
                                        !is.na(aggregated_sunday_data) ~ aggregated_sunday_data,
                                        TRUE ~ {{ agg_field }})) %>%
    dplyr::select(!c(aggregated_sunday_data))
  
  return(data)
}

#' @description moves deaths from bank holidays to the prior working day except
#'   for easter weekend deaths, which the Friday deaths are moved to the
#'   Thursday, and the Monday deaths are moved to the Tuesday note, deaths from
#'   the Jan 1st of the first year are removed as there are no prior working
#'   days note, function assumes weekend deaths are 0
bank_hols_to_nearest_work_day <- function(data, bank_holidays, date_field, agg_field) {
  easter_fridays <- calc_easter_fridays(bank_holidays)
  
  
  non_easter_holidays <- bank_holidays[!(bank_holidays %in% c(easter_fridays, (easter_fridays + 3)))]
  
  easter_data <- data %>%
    mutate(early_easter_group = {{ date_field }} %in% c(easter_fridays - 1, easter_fridays, easter_fridays + 1), # mark Thursday, Friday and Saturday around easter
           late_easter_group = {{ date_field }} %in% c(easter_fridays + 2, easter_fridays + 3, easter_fridays + 4)) # mark Sunday, Monday and Tuesday around easter
    
  
  early_easter_data <- easter_data %>%
    mutate(group_id = cumsum(1 - early_easter_group)) %>%
    filter(early_easter_group == TRUE) %>%
    group_by_at(vars(- {{ agg_field }}, 
                     - {{ date_field }},
                     -early_easter_group,
                     -late_easter_group)) %>%
    summarise({{ date_field }} := min({{ date_field }}),
              aggregated_early_easter_data = sum({{ agg_field }}),
              .groups = "drop") %>%
    dplyr::select(-group_id)
  
  late_easter_data <- easter_data %>%
    mutate(group_id = cumsum(1 - late_easter_group)) %>%
    filter(late_easter_group == TRUE) %>%
    group_by_at(vars(- {{ agg_field }}, 
                     - {{ date_field }},
                     -early_easter_group,
                     -late_easter_group)) %>%
    summarise({{ date_field }} := max({{ date_field }}),
              aggregated_late_easter_data = sum({{ agg_field }}),
              .groups = "drop") %>%
    dplyr::select(-group_id)
  
  any_non_easter_bhols <- any(non_easter_holidays %in% pull(data, {{ date_field }}))
  if (any_non_easter_bhols) {
    friday_bhols <- non_easter_holidays[wday(non_easter_holidays) == 6  & 
                                          month(non_easter_holidays) != 12]
    monday_bhols <- non_easter_holidays[wday(non_easter_holidays) == 2  & 
                                          month(non_easter_holidays) != 12]
    other_bhols <- non_easter_holidays[!(non_easter_holidays %in% c(friday_bhols, monday_bhols))]
    
    bank_hol_data <- tibble()
    
    # test for Friday bank holidays in data
    test_fri <- data %>%
      filter({{ date_field }} %in% friday_bhols) %>%
      nrow()
    
    if (test_fri > 0) {
      friday_bh <- data %>%
        mutate(day_of_week = wday({{ date_field }}),
               bh_data = {{ date_field }} %in% friday_bhols,
               mark_group = case_when(bh_data == TRUE | lead(bh_data == TRUE) ~ TRUE,
                                      TRUE ~ FALSE)) %>%
        mutate(group_id = cumsum(1 - mark_group)) %>%
        filter(mark_group == TRUE) %>%
        group_by_at(vars(- {{ agg_field }}, 
                         - {{ date_field }},
                         -bh_data,
                         -day_of_week,
                         -mark_group)) %>%
        summarise({{ date_field }} := min({{ date_field }}),
                  aggregated_bh_data = sum({{ agg_field }}),
                  .groups = "drop") %>%
        dplyr::select(-group_id)
      
      bank_hol_data <- bind_rows(bank_hol_data,
                                 friday_bh)
    }
    
    
    # test for Monday bank holidays in data
    test_mon <- data %>%
      filter({{ date_field }} %in% monday_bhols) %>%
      nrow()
    
    if (test_mon > 0) {
      monday_bh <- data %>%
        mutate(day_of_week = wday({{ date_field }}),
               bh_data = {{ date_field }} %in% monday_bhols,
               mark_group = case_when(bh_data == TRUE | lag(bh_data == TRUE) ~ TRUE,
                                      TRUE ~ FALSE)) %>%
        mutate(group_id = cumsum(1 - mark_group)) %>%
        filter(mark_group == TRUE) %>%
        group_by_at(vars(- {{ agg_field }}, 
                         - {{ date_field }},
                         -bh_data,
                         -day_of_week,
                         -mark_group)) %>%
        summarise({{ date_field }} := max({{ date_field }}),
                  aggregated_bh_data = sum({{ agg_field }}),
                  .groups = "drop") %>%
        dplyr::select(-group_id)
      
      bank_hol_data <- bind_rows(bank_hol_data,
                                 monday_bh)
    }
    
    # test for other bank hols in data
    test_other <- data %>%
      filter({{ date_field }} %in% other_bhols) %>%
      nrow()
    
    if (test_other > 0) {
      other_bh <- data %>%
      filter(!(wday({{ date_field }})) %in% c(1, 7)) %>%
      mutate(day_of_week = wday({{ date_field }}),
             bh_data = {{ date_field }} %in% other_bhols,
             mark_group = case_when(bh_data == TRUE | lead(bh_data == TRUE) ~ TRUE,
                                    TRUE ~ FALSE)) %>%
      mutate(group_id = cumsum(1 - mark_group)) %>%
      filter(mark_group == TRUE) %>%
      group_by_at(vars(- {{ agg_field }}, 
                       - {{ date_field }},
                       -bh_data,
                       -day_of_week,
                       -mark_group)) %>%
      summarise({{ date_field }} := min({{ date_field }}),
                aggregated_bh_data = sum({{ agg_field }}),
                .groups = "drop") %>%
      dplyr::select(-group_id)

      bank_hol_data <- bind_rows(bank_hol_data,
                                 other_bh)
    }
    
  }
  
  
  data <- data %>%
    left_join(early_easter_data, 
              by = intersect(names(.), names(early_easter_data))) %>%
    left_join(late_easter_data, 
              by = intersect(names(.), names(late_easter_data)))
  
  if (any_non_easter_bhols) {
    data <- data %>%
      left_join(bank_hol_data, 
                by = intersect(names(.), names(bank_hol_data))) %>%
      mutate({{ agg_field }} := case_when(wday({{ date_field }}) %in% c(1, 7) ~ 0,
                                          {{ date_field }} %in% bank_holidays ~ 0,
                                          !is.na(aggregated_early_easter_data) ~ aggregated_early_easter_data,
                                          !is.na(aggregated_late_easter_data) ~ aggregated_late_easter_data,
                                          !is.na(aggregated_bh_data) ~ aggregated_bh_data,
                                          TRUE ~ {{ agg_field }})) %>%
      dplyr::select(-aggregated_early_easter_data, -aggregated_late_easter_data, -aggregated_bh_data)
  } else {
    data <- data %>%
      mutate({{ agg_field }} := case_when(wday({{ date_field }}) %in% c(1, 7) ~ 0,
                                          {{ date_field }} %in% bank_holidays ~ 0,
                                          !is.na(aggregated_early_easter_data) ~ aggregated_early_easter_data,
                                          !is.na(aggregated_late_easter_data) ~ aggregated_late_easter_data,
                                          TRUE ~ {{ agg_field }})) %>%
      dplyr::select(-aggregated_early_easter_data, -aggregated_late_easter_data)
  }
  
  
  return(data)
}

#' @description change male to 0 and female to 1 because then modelling
#'   understands this as binary variable
sex_change_0_1 <- function(data, sex_field) {
  if (length(setdiff(unique(pull(data, {{ sex_field }})), c(1, 2))) == 0) {
    data <- data %>%
      mutate({{ sex_field }} := case_when({{ sex_field }} == 1 ~ 0,
                                                       {{ sex_field }} == 2 ~ 1,
                                                       TRUE ~ -1),
             {{ sex_field }} := factor({{ sex_field }}))
  }
  
  return(data)

}

#' @description aggregates tables that already contain calculated predictor
#'   variables up to higher geographies
summarise_baseline_to_higher_aggregation <- function(data) {
  data <- data %>%
    summarise(deaths_total = sum(deaths_total),
              denominator = sum(denominator),
              years_from_20161231 = mean(years_from_20161231),
              WedpreE = mean(WedpreE),
              ThurpreE = mean(ThurpreE),
              TuespostE = mean(TuespostE),
              WedpostE = mean(WedpostE),
              ThurpostE = mean(ThurpostE),
              FripostE = mean(FripostE),
              MonpostE1 = mean(MonpostE1),
              TuespostE1 = mean(TuespostE1),
              BH_nearest_WD = mean(BH_nearest_WD),
              BH_next_nearest_WD = mean(BH_next_nearest_WD),
              month1 = mean(month1),
              month2 = mean(month2),
              month3 = mean(month3),
              month4 = mean(month4),
              month5 = mean(month5),
              month6 = mean(month6),
              month7 = mean(month7),
              month8 = mean(month8),
              month9 = mean(month9),
              month10 = mean(month10),
              month11 = mean(month11),
              month12 = mean(month12),
              day1 = mean(day1),
              day2 = mean(day2),
              day3 = mean(day3),
              day4 = mean(day4),
              day5 = mean(day5))
  return(data)
}

#' @description retrieve a UTLA code, UTLA Name, Region code and Region Name
#'   lookup from the Data Lake
utla_lookup <- function() {
  con <- dbConnect(odbc(), 
                   Driver = "SQL Server", 
                   Server = Sys.getenv("DATA_LAKE_SERVER"), 
                   Database = Sys.getenv("LOOKUPS_DATABASE"), 
                   Trusted_Connection = "True",
                   timeout = 120)
  
  lkup <- tbl(con, in_schema(Sys.getenv("LOOKUPS_DATABASE"), Sys.getenv("UTLA19_LKP_TABLE")))
  lkup_table <- lkup %>%
    dplyr::select(UTLAApr19CD = UTLA19CD, UTLAApr19NM = UTLA19NM, RGN09CD, RGN09NM) %>%
    collect() %>%
    unique()
  dbDisconnect(con)
  return(lkup_table)
}

#' @description regional name looking
rgn_lookup <- function() {
  rgn_lookup <- tibble::tribble(
    ~area_name,                   ~OfficialCode,
    "North East",               "North East",
    "North West",               "North West",
    "Yorkshire and The Humber", "Yorkshire and The Humber",
    "East Midlands",            "East Midlands",
    "West Midlands",            "West Midlands",
    "East",                     "East of England",
    "London",                   "London",
    "South East",               "South East",
    "South West",               "South West"
  )
}

#' @description Manipulate deaths data so weekdays (and not bank hols) are
#'   included, along with sex only containing 0 and 1, and all data from bank
#'   hols and weekends are assigned to the previous working day. Also field
#'   added for years from end of Dec
preprocess_deaths <- function(data, date_field, death_field, sex_field, age_group_field, holidays) {
  
  data <- data %>%
    weekends_to_nearest_work_day(date_field = {{ date_field }}, 
                                 agg_field = {{ death_field }}) %>%
    bank_hols_to_nearest_work_day(bank_holidays = holidays, 
                                  date_field = {{ date_field }},
                                  agg_field = {{ death_field }}) %>%
    filter(!(wday({{ date_field }}) %in% c(1, 7)),
           !({{ date_field }} %in% holidays),
           Sex != 3) %>%
    mutate(day = wday({{ date_field }}), 
           month = floor_date({{ date_field }}, unit = "month")) %>%
    sex_change_0_1({{ sex_field }}) %>%
    mutate(years_from_20161231 = as.numeric({{ date_field }} - as.Date("2016-12-31")) / 365.25,
           {{ age_group_field }} := factor({{ age_group_field }})) %>% 
    rename(date = {{ date_field }})
  
  return(data)
}

#' @description creates distribution of expected values around the modelled
#'   value
#' @param n number of simulated values
#' @param mu expected value
#' @param dispersion_parameter dispersion parameter from model
#' @param var variance
rqpois <- function(n, mu, dispersion.parameter = NULL, var = NULL) {
  if (!is.null(dispersion.parameter)) {
    theta <- dispersion.parameter
  } else if (!is.null(var)) {
    theta <- var/mu
  } else {
    return("error, you must specify a value for either the var argument or the dispersion parameter argument")
  }
  
  rqpois <- rnbinom(n = n, mu = mu, size = mu/(theta-1))
  return(rqpois)
}

#' @description add lower and upper prediction interval fields to data frame
#' @param data data frame containing data
#' @param modelled_field unquoted field name of field in data containing the
#'   expected values
#' @inheritParams rqpois
add_prediction_intervals <- function(data, modelled_field, dispersion_parameter) {
  if (dispersion_parameter < 1) dispersion_parameter <- 1
  
  data <- data %>%
    mutate(random_total = map({{ modelled_field }}, ~rqpois(5000, .x, dispersion_parameter)),
           lpb = map_dbl(random_total, ~quantile(.x, 0.00135)),
           upb = map_dbl(random_total, ~quantile(.x, 0.99865))) %>%
    dplyr::select(-random_total)
  return(data)
}

add_prediction_intervals_tibble <- function(data, modelled_field, dispersion_parameter) {
  data <- data %>%
    mutate({{ dispersion_parameter }} := case_when(
              {{ dispersion_parameter }} < 1 ~ 1,
              TRUE ~ {{ dispersion_parameter }}),
           random_total = map({{ modelled_field }}, ~rqpois(5000, .x, {{ dispersion_parameter }})),
           lpb = map_dbl(random_total, ~quantile(.x, 0.00135)),
           upb = map_dbl(random_total, ~quantile(.x, 0.99865))) %>%
    dplyr::select(-random_total)
  return(data)
}

#' @description look up table for age in years to age group
age_group_lkp <- function(age_filter = NULL, type = "original") {
  
  if (!is.null(age_filter)) {
    ages <- convert_age_filter(age_filter)
  } else {
    ages <- list(lower_age = 0,
                 upper_age = 200)
  }
  
  if (type == "original") {
    agegroup_lkp <- tibble(Age = ages$lower_age:ages$upper_age) %>%
      mutate(Age_Group = case_when(
        Age < 15 ~ "0-14",
        Age < 45 ~ "15-44",
        Age < 65 ~ "45-64",
        Age < 75 ~ "65-74",
        Age < 85 ~ "75-84",
        TRUE ~ "85+"),
        Age_Group_PHA = case_when(
          Age < 15 ~ "<15",
          Age < 45 ~ "15-44",
          Age < 65 ~ "45-64",
          Age < 75 ~ "65-74",
          Age < 85 ~ "75-84",
          TRUE ~ "85+"
        ))
  } else if (type == "five year") {
    agegroup_lkp <- tibble(Age = ages$lower_age:ages$upper_age) %>%
      mutate(Age_Group = case_when(
        Age < 5 ~ "0-4",
        Age < 10 ~ "5-9",
        Age < 15 ~ "10-14",
        Age < 20 ~ "15-19",
        Age < 25 ~ "20-24",
        Age < 30 ~ "25-29",
        Age < 35 ~ "30-34",
        Age < 40 ~ "35-39",
        Age < 45 ~ "40-44",
        Age < 50 ~ "45-49",
        Age < 55 ~ "50-54",
        Age < 60 ~ "55-59",
        Age < 65 ~ "60-64",
        Age < 70 ~ "65-69",
        Age < 75 ~ "70-74",
        Age < 80 ~ "75-79",
        Age < 85 ~ "80-84",
        Age < 90 ~ "85-89",
        TRUE ~ "90+"))
  } else if (type == "nomis") {
    agegroup_lkp <- tibble(Age = ages$lower_age:ages$upper_age) %>%
      mutate(Age_Group = case_when(
        Age < 25 ~ "0-24",
        Age < 50 ~ "25-49",
        Age < 65 ~ "50-64",
        Age < 75 ~ "65-74",
        Age < 85 ~ "75-84",
        TRUE ~ "85+"),
        Age_Group_Nomis = case_when(
          Age < 25 ~ "Age 0 to 24",
          Age < 50 ~ "Age 25 to 49",
          Age < 65 ~ "Age 50 to 64",
          Age < 75 ~ "Age 65 to 74", #this doesn't exist in Nomis data
          Age < 85 ~ "Age 75 to 84", #this doesn't exist in Nomis data
          TRUE ~ "Age 85 and over")) #this doesn't exist in Nomis data
  }
  
  return(agegroup_lkp)
}

convert_age_filter <- function(age_filter) {
  lower_age <- age_filter[1]
  if (is.na(lower_age)) lower_age <- 0
  
  upper_age <- age_filter[2]
  if (is.na(upper_age)) upper_age <- 200
  
  return(list(lower_age = lower_age,
              upper_age = upper_age))
}

#' @description removes records from the start of data frame where they are bank
#'   holidays
#' @param data data frame
#' @param date_field unquoted field name in data containing dates
#' @param holidays date vector of holidays
adjust_first_date <- function(data, date_field, holidays) {
  first_date <- data %>%
    mutate(hols_we = {{ date_field }} %in% holidays | 
             wday({{ date_field }}) %in% c(1, 7)) %>%
    filter(hols_we == FALSE) %>%
    slice(1) %>%
    pull({{ date_field }})
  
  data <- data %>%
    filter({{ date_field }} >= first_date)
  
  return(data)
}


#' @description removes records from the end of data frame where they are bank
#'   holidays
#' @inheritParams adjust_first_date
adjust_last_date <- function(data, date_field, holidays) {
  easter_monday <- calc_easter_fridays(holidays) + 3
  last_date <- data %>%
    mutate(sun_easter_mon = {{ date_field }} %in% easter_monday | 
             wday({{ date_field }}) %in% c(1)) %>%
    filter(sun_easter_mon == FALSE) %>%
    tail(1) %>%
    pull({{ date_field }})
  
  data <- data %>%
    filter({{ date_field }} <= last_date)
  
  return(data)
}

#' @description generates unique file name based on modelling criteria
#' @inheritParams get_predictions
generate_file_name <- function(model_filename,  
                               directory = Sys.getenv("PREDICTIONS_FILESHARE"),
                               visualisation_geography = NULL, facet_fields = "",
                               age_filter = NULL) {
  model_filename <- basename(model_filename)
  model_filename <- tools::file_path_sans_ext(model_filename)
  
  if (!is.null(visualisation_geography)) model_filename <- paste(model_filename,
                                                                 visualisation_geography,
                                                                 sep = "_")
  if (!(facet_fields == "")) model_filename <- paste(model_filename,
                                                     facet_fields,
                                                     sep = "_")
  
  if (!is.null(age_filter)) model_filename <- paste(model_filename,
                                                    paste(age_filter, collapse = "_"),
                                                    sep = "_")
  
  model_filename  <- paste(model_filename,
                           "ons_aligned_weekly",
                           sep = "_")
  
  filename <- paste0(directory,
                     "/",
                     model_filename,
                     ".csv")
  return(filename)
}

#' @description ethnic group lookup table
ethnic_groups_lookup <- function() {
  ethnic_groups <- tibble::tribble(
    ~Ethnic_Group,                       ~Ethnic_group_data_lake,                                                                         ~Ethnicity_Broad,                               ~Ethnicity_Nomis,
    "Asian",                       "Asian / Asian British",                                             "2-Asian or Asian British including Chinese",                   "Asian/Asian British: Total",
    "Black", "Black / African / Caribbean / Black British",                                                               "3-Black or Black British", "Black/African/Caribbean/Black British: Total",
    "Mixed",              "Mixed / Multiple ethnic groups",                                                                                "4-Mixed",           "Mixed/multiple ethnic group: Total",
    "Not_Stated/Not_Known/Not_Given",                    "No ethnicity information",                                                       "6-Not_Stated/Not_Known/Not_Given",                                             NA,
    "Other",                      "Any other ethnic group",                                                                  "5-Other Ethnic Groups",                    "Other ethnic group: Total",
    "Unknown",                    "No ethnicity information", "7-NULL-SEX in mortality does match SEX in HES or Ethnicity not in HES for search FYEAR",                                             NA,
    "White",                                       "White",                                                                                "1-White",                                 "White: Total"
  )
  
  return(ethnic_groups)
}

#' @description build a data frame for prediction dates with same structure as
#'   modelled data that model can be used to predict deaths on
#' @param area_codes character vector of area codes to generate data frame for
#' @inheritParams create_baseline
#' @inheritParams generate_visualisations
#' @param utla_lkp a tibble lookup table for utla to region (see utla_lookup())
build_recent_dates <- function(area_codes, from_date, to_date, holidays, denominators, utla_lkp, eth_dep, 
                               age_filter = NULL, age_group_type = "original") {
  utlas <- utla_lkp %>%
    filter(!(UTLAApr19CD %in% c("E06000053", "E09000001"))) %>%
    pull(UTLAApr19CD)
  
  rgns <- utla_lkp %>%
    pull(RGN09CD) %>%
    unique()
  
  Sex <- factor(c(0, 1))
  
  Age_Group <- age_group_lkp(age_filter = age_filter, type = age_group_type) %>%
    pull(Age_Group) %>%
    unique() %>%
    factor()
  
  recent_dates <- data.frame(date = seq(from = from_date, 
                                        to = to_date,
                                        by = 'days')) %>% 
    filter(!(wday(date) %in% c(1, 7)),
           !(date %in% holidays)) 
  
  if (eth_dep == TRUE) {
    recent_dates <- recent_dates %>%
      merge(y = data.frame(RGN09CD = area_codes, stringsAsFactors = FALSE)) %>%
      merge(y = data.frame(Ethnic_Group = factor(c("Asian", "Black", "Mixed", "Other", "White")))) %>%
      merge(y = data.frame(Deprivation_Quintile = factor(1:5)))
    
  } else {
    recent_dates <- recent_dates %>%
      merge(y = data.frame(UTLAApr19CD = utlas, stringsAsFactors = FALSE))
  }
  
  recent_dates <- recent_dates  %>%
    merge(y = data.frame(Sex = Sex)) %>%
    merge(y = data.frame(Age_Group = Age_Group)) %>%
    mutate(deaths_total = NA, 
           month = floor_date(date, unit = "month"), 
           years_from_20161231 = as.numeric(date - as.Date("2016-12-31")) / 365.25)
  
  if (eth_dep == TRUE) {
    recent_dates <- recent_dates %>%
      left_join(denominators, by = c("RGN09CD" = "OfficialCode", "Ethnic_Group", "Deprivation_Quintile", "Sex", "Age_Group", "month"))  
    
  } else {
    recent_dates <- recent_dates %>%
      left_join(denominators, by = c("UTLAApr19CD" = "OfficialCode", "Sex", "Age_Group", "month"))
    
    
  }
  recent_dates <- recent_dates %>%
    dplyr::select(-month)
  
  # calculate easter Fridays
  easter_fridays <- calc_easter_fridays(as.Date(holidays))
  non_easter_non_xmas_hols <- calc_non_easter_non_xmas_hols(holidays, easter_fridays)
  
  recent_dates <- recent_dates %>%
    mutate(day = wday(date)) %>%
    add_easter_binary_variables(date, easter_fridays) %>%
    add_bh_binary_variables(date_field = date, day_field = day, non_easter_non_xmas_hols) %>%
    add_day_weighting(date_field = date) %>% # so column order is sensible when following pivot_wider
    pivot_wider(names_from = month,
                names_prefix = "month",
                values_from = month_val,
                values_fill = list(month_val = 0)) %>%
    arrange(date) %>% 
    mutate(day1 = if_else(day == "2", 1, 0),
           day2 = if_else(day == "3", 1, 0),
           day3 = if_else(day == "4", 1, 0),
           day4 = if_else(day == "5", 1, 0),
           day5 = if_else(day == "6", 1, 0)) %>% 
    dplyr::select(-day) %>%
    ungroup()
  
  
  recent_dates <- recent_dates %>% 
    mutate(deaths_total = NA) %>% 
    convert_to_weekly_for_modelling(from_date = from_date,
                                    to_date = to_date)  
  
  expected_months <- paste0("month", 1:12)
  missing_months <- setdiff(expected_months,
                            names(recent_dates))
  recent_dates[missing_months] <- 0
  return(recent_dates)
}

#' @description rounding function
#' @details from https://stackoverflow.com/questions/12688717/round-up-from-5
round_correct <- function(x, n) {
  posneg <- sign(x)
  z <- abs(x) * 10 ^ n
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10 ^ n
  z <- z * posneg
  
  return(z)
}

#' @description place of death lookup table
pod_lookup <- function() {
  pod_lkp <- tibble::tribble(
    ~pod_input,                                      ~pod_filter,
    "hospital", "Hospital (acute or community, not psychiatric)",
    "care home",             "Care home (nursing or residential)",
    "other",                                   "Other places",
    "hospice",                                        "Hospice",
    "home",                                           "Home"
  )
  return(pod_lkp)
}

#' @description transform age groups for visualisations
transform_age_groups <- function(data, age_group_field, age_groups, apply_labels = TRUE) {
  
  if (apply_labels == TRUE) {
    data <- data %>%
      mutate({{ age_group_field }} := factor({{ age_group_field }},
                                             levels = age_groups,
                                             labels = paste("Age group\n",
                                                            age_groups,
                                                            "(years)")))
  } else {
    data <- data %>%
      mutate({{ age_group_field }} := factor({{ age_group_field }},
                                             levels = age_groups))
  }
  
  return(data)
}

#' @description transform ethnic groups for visualisations
transform_ethnic_groups <- function(data, ethnic_group_field, ethnic_groups, apply_labels = TRUE) {
  
  if (apply_labels == TRUE) {
    data <- data %>%
      mutate({{ ethnic_group_field }} := factor({{ ethnic_group_field }},
                                                levels = ethnic_groups,
                                                labels = paste("Ethnic group\n",
                                                               ethnic_groups)))
  } else {
    data <- data %>%
      mutate({{ ethnic_group_field }} := factor({{ ethnic_group_field }},
                                                levels = ethnic_groups))
  }
  
  return(data)
}

#' @description transform sex groups for visualisations
transform_sex_groups <- function(data, sex_field) {
  data <- data %>%
    mutate({{ sex_field }} := factor({{ sex_field }},
                                     levels = as.character(0:1),
                                     labels = c("Males", "Females")))
  return(data)
}

#' @description transform deprivation groups for visualisations
transform_deprivation_groups <- function(data, deprivation_field) {
  data <- data %>%
    mutate({{ deprivation_field }} := factor({{ deprivation_field }},
                                     levels = as.character(1:5),
                                     labels = c("Quintile 1 - most deprived",
                                                "Quintile 2",
                                                "Quintile 3",	
                                                "Quintile 4",
                                                "Quintile 5 - least deprived")))
  return(data)
}

#' @description transform regions for visualisations
transform_regions <- function(data, region_field) {
  region_levels <- c("North East", 
                     "North West",
                     "Yorkshire and The Humber",
                     "East Midlands",
                     "West Midlands",
                     "East of England",
                     "London",
                     "South East",
                     "South West")
  data <- data %>%
    mutate({{ region_field }} := factor({{ region_field }},
                                     levels = region_levels))
  return(data)
}

#' @description a cause code to disease lookup table
#' @param cause_name character vector to filter output with
#' @param table_output logical; FALSE returns vector of ICD codes, TRUE returns
#'   a table with cause_name and ICD codes
cause_code_lookup <- function(cause_name = NULL, table_output = FALSE,
                              include_epi_hf = FALSE) {
  ihd_codes <- paste0("I", 20:25)
  cereberovascular_codes <- paste0("I", 60:69)
  other_circulatory_codes <- paste0("I", formatC(0:99, width = 2, flag = "0"))
  other_circulatory_codes <- other_circulatory_codes[!(other_circulatory_codes %in% c(ihd_codes,
                                                                                      cereberovascular_codes))]
  cancer_codes <- paste0("C", formatC(0:97, width = 2, flag = "0"))
  ari_codes <- paste0("J", formatC(0:22, width = 2, flag = "0"))
  clr_codes <- paste0("J", 40:47)
  other_respiratory_codes <- paste0("J", formatC(0:99, width = 2, flag = "0"))
  other_respiratory_codes <- other_respiratory_codes[!(other_respiratory_codes %in% c(ari_codes,
                                                                                      clr_codes))]
  dementia_codes <- c("F01", "F03", "G30")
  urinary_system_codes <- paste0("N", formatC(0:39, width = 2, flag = "0"))
  cirrhosis_liver_codes <- paste0("K", 70:76)
  parkinsons_codes <- "G20"
  diabetes_codes <- paste0("E", 10:14)
  
  covid_codes <- c("U07")
  
  
  cause_codes <- expand.grid(letter = LETTERS, 
                             number = formatC(0:99, width = 2, flag = "0")) %>%
    mutate(ICD_codes = paste0(letter, number)) %>%
    dplyr::select(ICD_codes) %>%
    bind_rows(tibble(ICD_codes = "")) %>%
    mutate(name_of_cause = case_when(
             ICD_codes %in% ihd_codes ~ "ischaemic heart diseases",
             ICD_codes %in% cereberovascular_codes ~ "cerebrovascular diseases",
             ICD_codes %in% other_circulatory_codes ~ "other circulatory diseases",
             ICD_codes %in% cancer_codes ~ "cancer",
             ICD_codes %in% ari_codes ~ "acute respiratory infections",
             ICD_codes %in% clr_codes ~ "chronic lower respiratory diseases",
             ICD_codes %in% other_respiratory_codes ~ "other respiratory diseases",
             ICD_codes %in% dementia_codes ~ "dementia and Alzheimer's",
             ICD_codes %in% urinary_system_codes ~ "diseases of the urinary system",
             ICD_codes %in% cirrhosis_liver_codes ~ "cirrhosis and other liver diseases",
             ICD_codes %in% parkinsons_codes ~ "Parkinson's disease",
             ICD_codes %in% diabetes_codes ~ "diabetes",
             ICD_codes %in% covid_codes ~ "COVID-19",
             TRUE ~ "All other causes (excl. COVID-19)"
           ))
  
  if (include_epi_hf == TRUE) {
    additional_codes <- read.csv("data/additional_icd_codes.csv")
    
    cause_codes <- cause_codes %>% 
      filter(name_of_cause != "All other causes (excl. COVID-19)") %>% 
      bind_rows(additional_codes)
  }
  
  if (table_output == TRUE) {
    return(cause_codes)
  } else {
    cause_codes <- cause_codes %>%
      filter(name_of_cause %in% cause_name) %>%
      pull(ICD_codes)
    
    if (length(cause_codes) == 0) stop("cause_name does not match reference in the cause_code_lookup function")
    return(cause_codes)
  }
}

#' @description helper function to convert rds object for model filename to txt
#'   file to store the dispersion parameter
#' @inheritParams get_predictions
filename_model_to_dispersion <- function(model_filename) {
  disp_par_path <- Sys.getenv("DISPERSION_PARAMETERS_FILESHARE")
  disp_par_filename <- gsub("rds$", "txt", model_filename)
  disp_par_filename <- gsub("model_outputs", disp_par_path, disp_par_filename)
  return(disp_par_filename) 
}



#' @description check to ensure visualisation_geography used is within range for
#'   given combination of ethnicity and deprivation (as models are built at
#'   different geographies)
#' @inheritParams generate_visualisations
vis_geography_variable <- function(visualisation_geography, eth_dep) {
  if (!eth_dep == TRUE) {
    if (!(visualisation_geography %in% c("region", "england"))) stop("visualisation_geography must be either region, england")
  } else {
    if (!(visualisation_geography %in% c("utla", "region", "england"))) stop("visualisation_geography must be either utla, region, england")
  }
  
  if (visualisation_geography == "utla") {
    return("UTLAApr19CD")
  } else if (visualisation_geography == "region") {
    return("RGN09CD") 
  } else {
    return(NULL)
  }
}

#' @description check to ensure facet_variables entered is within possible range
#' @inheritParams generate_visualisations
facet_variables <- function(facet_fields = NULL, eth_dep) {
  if (is.null(facet_fields)) return(NULL)
  
  # if (!(names(facet_fields) %in% c("x", "y"))) stop("names of facet_fields must be x or y")
  
  if ("Ethnic_Group" %in% facet_fields & eth_dep == FALSE) 
    stop("ethnicity must be equal to TRUE if Ethnic_Group one of facet_fields items")
  
  if ("Deprivation_Quintile" %in% facet_fields & eth_dep == FALSE) 
    stop("deprivation must be equal to TRUE if Deprivation_Quintile is one of facet_fields items")
  
  if (!(any(facet_fields %in% c("Deprivation_Quintile", "Ethnic_Group", "Sex", "Age_Group", "RGN09CD", "UTLAApr19CD", "POD_out", "name_of_cause"))))
    stop("facet_fields must be one or more of the following; Deprivation_Quintile, Ethnic_Group, Sex, Age_Group, RGN09CD, UTLAApr19CD, POD_out, name_of_cause")
  
  return(facet_fields)
      
}

#' @description chect that split_chart exists in grouping_fields
split_chart_variables <- function(field, grouping_fields) {
  if (!(field %in% grouping_fields)) stop("chart splitting field isn't in the grouping fields created by facet_fields")
  
  return(field)
}

#' @description export data ready for using in PowerBI
export_powerbi_data <- function(data, grouping_fields, model_filename, from_date, end_date,
                                directory = Sys.getenv("POWERBI_FILESHARE"), age_filter = NULL,
                                breakdown_by_ucod = FALSE) {
  # create directory if it doesn't exist already
  dir.create(directory, showWarnings = FALSE)
  
  if (length(model_filename) > 1) {
    model_filename <- ""
  } else {
    if (length(grouping_fields) > 1) {
      model_filename <- paste0(model_filename, "_")  
    }
  }
  
  
  grouping_fields_without_date <- grouping_fields[grouping_fields != "date"]
  grouping_fields_without_date_with_weekid <- c(grouping_fields_without_date, "weekid")
  
  all_dates <- tibble(date = seq(from = from_date,
                                 to = max(data$date),
                                 by = "days"))
  
  grouping_vars <- data %>% 
    distinct(across(all_of(grouping_fields_without_date)))
  
  all_dates <- all_dates %>% 
    merge(grouping_vars, all.x = TRUE, all.y = TRUE)
  
  if (length(grouping_fields_without_date) > 0) {
    data <- data %>% 
      group_by(across(all_of(grouping_fields_without_date)))
      
  }
  
  week_end <- 7
  
  data <- data %>% 
    full_join(all_dates, by = intersect(names(.), names(all_dates))) %>% 
    arrange(across(all_of(grouping_fields))) %>% 
    mutate(last_day = wday(date) == week_end,
           weekid = cumsum(last_day)) %>% 
    ungroup() %>% 
    group_by(across(all_of(c(grouping_fields_without_date_with_weekid, "weekid"))))
  
  if (breakdown_by_ucod == TRUE) {
    data <- data %>% 
      summarise(date = max(date),
                registered = sum(registered_count, na.rm = TRUE),
                expected = sum(modelled_deaths_zeros, na.rm = TRUE),
                ucod_covid = sum(deaths_total, na.rm = TRUE),
                ucod_disease = sum(ucod_disease, na.rm = TRUE),
                days_in_week = n(),
                .groups = "drop")
  } else if (breakdown_by_ucod == FALSE) {
    data <- data %>%
      summarise(date = max(date),
                registered = sum(registered_count, na.rm = TRUE),
                expected = sum(modelled_deaths_zeros, na.rm = TRUE),
                covid = sum(deaths_total, na.rm = TRUE),
                days_in_week = n(),
                .groups = "drop")
  }
  data <- data %>%
    filter(date <= end_date) %>% 
    dplyr::select(-weekid)
  
  if (!is.null(age_filter)) {
    filename <- paste0(directory, "/", model_filename,
                       paste(grouping_fields_without_date, collapse = "_"),
                       "_",
                       paste(age_filter, collapse = "_"))
  } else {
    filename <- paste0(directory, "/", model_filename,
                       paste(grouping_fields_without_date, collapse = "_"))
  }
  
  filename  <- paste(filename,
                     "ons_aligned_weekly.csv",
                     sep = "_")
  write.csv(data, 
            filename,
            row.names = FALSE)
}

#' @description generate date to use for final date in report
final_report_date <- function() {
  final_date <- Sys.Date() - lubridate::wday(Sys.Date() + 1) - 7
  return(final_date)
}


wordify_ratio <- function(ratio) {
  rounded <- round_correct(ratio / 0.5, 0) * 0.5
  remainder <- rounded %% 1 != 0
  floor_0 <- floor(rounded)
  
  if (ratio > rounded) {
    txt1 <- "over"
  } else if (ratio < rounded) {
    txt1 <- "nearly"
  } else {
    txt1 <- ""
  }
  
  if (floor_0 == 1) {
    txt2 <- "one"
  } else if (floor_0 == 2) {
    txt2 <- "two"
  } else if (floor_0 == 3) {
    txt2 <- "three"
  } else if (floor_0 == 4) {
    txt2 <- "four"
  } else {
    stop("number not recognised")
  }
  
  if (remainder) {
    txt_out <- paste(txt1, txt2, "and a half")
  } else {
    txt_out <- paste(txt1, txt2)
  }
  
  return(txt_out)
}


transform_nomis_ages <- function(data, age_field, LSOA_field, pop_field) {
  keep_names <- replace(names(data), names(data) == "C_AGE_NAME", "Age_Group")
  
  old_age <- data %>% 
    filter({{ age_field }} == "Age 65 and over")
  
  data <- data %>% 
      filter({{ age_field }} != "Age 65 and over")
  
  age_gp_lkp_young <- age_group_lkp(type = "nomis") %>% 
    distinct(Age_Group_Nomis, Age_Group)
  
  age_gp_lkp_older <- age_group_lkp(type = "original")
  # https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/008781populationdenominatorsbybroadethnicgroupandforwhitebritishlocalauthoritiesinenglandandwales2011to2017
  pops_url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/008781populationdenominatorsbybroadethnicgroupandforwhitebritishlocalauthoritiesinenglandandwales2011to2017/localdenominators2019.xls"
  GET(pops_url, write_disk(tf <- tempfile(fileext = ".xls")))
  
  lad_proportions <- readxl::read_excel(tf,
                                        sheet = "2019") %>% 
    filter(age >= 65,
           grepl("^E", lad2014_code)) %>% 
    left_join(age_gp_lkp_older, by = c("age" = "Age")) %>% 
    group_by(lad2014_code, Age_Group, sex) %>% 
    summarise(White = sum(White),
              Mixed = sum(Mixed),
              Asian = sum(Asian),
              Black = sum(Black),
              Other = sum(Other),
              .groups = "drop") %>% 
    dplyr::select(lad2014_code, Age_Group, Sex = sex, White, Mixed, Asian, Black, Other) %>% 
    pivot_longer(cols = c(White, Mixed, Asian, Black, Other),
                 names_to = "Ethnic_Group",
                 values_to = "denominator") %>% 
    group_by(lad2014_code, Ethnic_Group, Sex) %>% 
    add_tally(denominator) %>% 
    mutate(proportions = case_when(
                          n == 0 ~ 0,
                          TRUE ~ denominator / n)) %>% 
    dplyr::select(-c(denominator, n))
    
  # get lookup from LSOA to LAD2014
  con <- dbConnect(odbc(), 
                   Driver = "SQL Server", 
                   Server = Sys.getenv("DATA_LAKE_SERVER"), 
                   Database = Sys.getenv("POPULATIONS_DATABASE"), 
                   Trusted_Connection = "True",
                   timeout = 60)
  
  area_lkup <- tbl(con, in_schema(Sys.getenv("LOOKUPS_DATABASE"), Sys.getenv("LSOA_DEMOGRAPHICS_TABLE"))) %>% 
    dplyr::select(GEOGRAPHY_CODE = LSOA11CD, LTLA13CD) %>% 
    collect()
  
  dbDisconnect(con)
  
  # calculate older age bands using lad proportions
  old_age <- old_age %>% 
    left_join(area_lkup, by = intersect(names(.), names(area_lkup))) %>% 
    left_join(lad_proportions, by = c("LTLA13CD" = "lad2014_code", "C_SEX" = "Sex", "Ethnic_Group")) %>% 
    mutate({{ pop_field }} := {{ pop_field }} * proportions) %>% 
    dplyr::select(all_of(keep_names))
  
  data <- data %>% 
    left_join(age_gp_lkp_young, by = c("C_AGE_NAME" = "Age_Group_Nomis")) %>% 
    dplyr::select(all_of(keep_names)) %>% 
    bind_rows(old_age)
  
  return(data)
  
}


collate_powerbi_files <- function(remove_expected_registered = FALSE, 
                                  geography = c("england", "region")) {
  
  # check geography argument
  geography <- match.arg(geography)
  
  # create strings to find correct file names
  rgn_part_suffix <- ""
  rgn_part_prefix <- ""
  if (geography == "region") {
    rgn_part_suffix <- "_RGN09CD"
    rgn_part_prefix <- "RGN09CD_"
  }
  
  folder_path <- Sys.getenv("POWERBI_FILESHARE")
  
  suffix <- "_ons_aligned_weekly"
  
  combined_models <- tibble(
    reference = c("Place of death"),
    prediction_file = c("POD_out")
  )
  
  # create regex strings to remove from filenames depending on geography
  if (geography == "england") {
    filter_string_pattern <- "UCOD$|POD$|UTLA|Ethnicity-deprivation"
  } else if (geography == "region") {
    filter_string_pattern <- "UCOD$|POD$|Ethnicity-deprivation|England"
  }
  
  # create named vector of filenames where individual powerbi files are stored
  file_references <- all_models() %>%
    filter(!grepl(filter_string_pattern, reference)) %>%
    bind_rows(combined_models) %>%
    mutate(powerbi = case_when(
      grepl("POD_out|name_of_cause", prediction_file) ~ paste0(rgn_part_prefix,
                                                               prediction_file),
      TRUE ~ paste0(prediction_file,
                    rgn_part_suffix)),
           powerbi = case_when(
      grepl("Region", reference) & geography == "england" ~ paste(powerbi, "RGN09CD", sep = "_"),
      grepl("Age-Sex", reference) ~ paste(powerbi, "Age_Group_Sex", sep = "_"),
      grepl("Deprivation", reference) ~ paste(powerbi, "Deprivation_Quintile", sep = "_"),
      grepl("Ethnicity-Sex", reference) ~ paste(powerbi, "Ethnic_Group_Sex", sep = "_"),
      grepl("UTLA", reference) ~ paste(powerbi, "UTLAApr19CD", sep = "_"),
      TRUE ~ powerbi),
           powerbi = paste0(Sys.getenv("POWERBI_FILESHARE"),
                            "/",
                            powerbi, 
                            suffix,
                            ".csv")) %>%
    dplyr::select(-prediction_file) %>%
    tibble::deframe()

  # read in and collate the powerbi files
  # do some formatting and tidying up
  collated_output <- lapply(file_references, read.csv) %>%
    bind_rows(.id = "Chart_Name") %>%
    filter(days_in_week == 7) %>%
    mutate(excess_deaths = round_correct(registered - expected, 0),
           ratio = round_correct(registered / expected, 2),
           name_of_cause = case_when(
             grepl("mentions", Chart_Name) ~ Chart_Name,
             TRUE ~ NA_character_),
           name_of_cause = capitalise_first_letter(name_of_cause),
           Chart_Name = case_when(
             grepl("mentions", Chart_Name) ~ "Mention of cause of death",
             TRUE ~ Chart_Name)) %>%
    dplyr::select(!c(days_in_week)) %>%
    rename(Place_of_Death = POD_out,
           Week_End = date,
           Mention_of_Covid = covid,
           Cause_Name = name_of_cause,
           Region_Code = RGN09CD) %>% 
    relocate(registered, expected, Mention_of_Covid,
             .before = excess_deaths) %>% 
    rename(Registered = registered,
           Expected = expected,
           Excess_Deaths = excess_deaths)

  if (geography == "region") {
    # regional reports don't present weekly data, so this section aggregates appropriately
    chart_variables <- setdiff(names(collated_output),
                               c("Week_End", "ratio", 
                                 "Registered", "Expected",
                                 "Mention_of_Covid", "Excess_Deaths",
                                 "ucod_covid", "ucod_disease"))
    
    collated_output <- collated_output %>% 
      mutate(Week_End = as.Date(Week_End))
    
    dps <- obtain_dispersion_parameters() %>% 
      mutate(Chart_Name = capitalise_first_letter(Chart_Name))
    
    cumulative_output <- collated_output %>% 
      filter(Chart_Name != "Region") %>% 
      group_by(across(all_of(chart_variables))) %>% 
      summarise(Period_Start = range(Week_End)[1] - 6,
                Period_End = range(Week_End)[2],
                Registered = sum(Registered),
                Expected = sum(Expected),
                Mention_of_Covid = sum(Mention_of_Covid),
                Excess_Deaths = Registered - Expected,
                ucod_covid = sum(ucod_covid),
                ucod_disease = sum(ucod_disease),
                .groups = "keep") %>% 
      mutate(ratio = round_correct(Registered / Expected, 2),
             across(all_of(c("Registered", "Expected", 
                             "Mention_of_Covid", "Excess_Deaths")),
                    round_correct, n = 0),
             Cause_Name = str_remove(Cause_Name, "^All "),
             Cause_Name = capitalise_first_letter(Cause_Name),
             join_field = case_when(
               !is.na(Cause_Name) ~ Cause_Name,
               !is.na(Place_of_Death) ~ Place_of_Death,
               TRUE ~ Chart_Name)) %>% 
      left_join(dps, by = c("join_field" = "Chart_Name")) %>% 
      add_prediction_intervals_tibble(modelled_field = Expected,
                                      dispersion_parameter = Dispersion_Parameter) %>% 
      mutate(ratio = case_when(
        Registered > upb | Registered < lpb ~ ratio,
        TRUE ~ NA_real_)) %>% 
      dplyr::select(!c(Dispersion_Parameter, join_field, upb, lpb))
    
    collated_output <- collated_output %>% 
      filter(Chart_Name == "Region") %>% 
      rename(
        "Period_End" = Week_End
      ) %>% 
      mutate(Period_Start = Period_End - 6) %>% 
      bind_rows(cumulative_output) %>% 
      rename(
        "Underlying_cause_covid" = ucod_covid,
        "Underlying_cause_disease" = ucod_disease
      ) %>%
      relocate(Period_Start, 
               .before = Period_End)
  }
  
  if (remove_expected_registered == TRUE) {
    collated_output <- collated_output %>%
      dplyr::select(!c(Registered, Expected))
  }

  # write to excel and csv, returns filepath
  write_data_server(
    geography = geography, 
    data = collated_output,
    folder_path = folder_path
  )
  
}

obtain_dispersion_parameters <- function() {
  folder_path <- Sys.getenv("DISPERSION_PARAMETERS_FILESHARE")
  
  file_references <- all_models() %>% 
    mutate(prediction_file = paste0(folder_path, "/", prediction_file, ".txt")) %>% 
    tibble::deframe()
  
  collated_output <- lapply(file_references, readLines) %>% 
    bind_rows(.id = "Chart_Name")
  
  dispersion_parameters <- t(collated_output) %>% 
    data.frame() %>% 
    rename(Dispersion_Parameter = ".") %>% 
    tibble::rownames_to_column(var = "Chart_Name") %>% 
    mutate(Chart_Name = gsub(" POD| UCOD", "", Chart_Name),
           Dispersion_Parameter = as.numeric(Dispersion_Parameter))
  
  return(dispersion_parameters)
}

all_models <- function(model_name = NULL) {
  
  model_refs <- tibble::tribble(
    ~reference,                                                                          ~prediction_file,
    # "ARI mentions",                                                                "20210113_ari_mentions",
    # "Dementia mentions",                                                      "20210113_dementia_mentions",
    # "Other causes (excl. COVID-19) mentions",                                      "20210113_all_other_causes", 
    # "Epilepsy mentions",                                                      "20210302_epilepsy_mentions", 
    "Parkinson's disease mentions",                                 "20210302_parkinsons_disease_mentions", 
    "Diabetes mentions",                                                      "20210113_diabetes_mentions",
    "Cirrhosis and other liver diseases mentions",  "20210302_cirrhosis_and_other_liver_diseases_mentions", 
    "Diseases of the urinary system mentions",          "20210302_diseases_of_the_urinary_system_mentions", 
    "Dementia and Alzheimer's mentions",                       "20210302_dementia_and_alzheimers_mentions", 
    "Other respiratory diseases mentions",                  "20210302_other_respiratory_diseases_mentions", 
    "Chronic lower respiratory diseases mentions",  "20210302_chronic_lower_respiratory_diseases_mentions", 
    "Acute respiratory infections mentions",              "20210302_acute_respiratory_infections_mentions", 
    "Cancer mentions",                                                          "20210302_cancer_mentions",
    "Heart failure mentions",                                            "20210302_heart_failure_mentions",
    "Other circulatory diseases mentions",                  "20210302_other_circulatory_diseases_mentions",
    "Cerebrovascular diseases mentions",                      "20210302_cerebrovascular_diseases_mentions", 
    "Ischaemic heart diseases mentions",                      "20210302_ischaemic_heart_diseases_mentions",                  
    "Other places POD",                                                            "20210113_other_places",
    "Hospice POD",                                                                      "20210113_hospice",
    "Hospital (acute or community, not psychiatric) POD",                              "20210113_hospital",
    "Care home (nursing or residential) POD",                                         "20210113_care_home",
    "Home POD",                                                                            "20210113_home",
    "England",                                                                          "20210212_eth_dep",
    "Region",                                                                           "20210212_eth_dep",
    "Age-Sex",                                                                          "20210212_eth_dep",
    "Deprivation",                                                                      "20210212_eth_dep",
    "Ethnicity-Sex",                                                                    "20210212_eth_dep",
    "Ethnicity-deprivation",                                                            "20210212_eth_dep",
    "UTLA",                                                                                "20210113_utla")
  
                                                                                
  
  
  
  
  if (!is.null(model_name)) {
    if (!(model_name %in% model_refs$reference)) stop("model_name not in model references")
    model_refs <- model_refs %>% 
      filter(reference == model_name) %>% 
      pull(prediction_file)
  }
  return(model_refs)
}


#' @description lookup table for models used and diseases/place of death to
#'   ensure consistency in predictions and visualisations at England and
#'   regional level
model_references <- function() {
  
  references <- all_models() %>% 
    filter(grepl("mentions$|POD$", reference)) %>% 
    left_join(icd_captions(), by = "reference") %>% 
    mutate(model_file = paste0("model_outputs\\",
                               prediction_file, 
                               ".rds"),
           reference = case_when(
             grepl("Other causes \\(excl\\. COVID-19\\)", reference) ~ "All other causes (excl. COVID-19)",
             TRUE ~ reference),
           reference = gsub(" mentions$| POD$", "", reference)) %>% 
    dplyr::select(reference, model_file, caption)
  
  return(references)
}


#' @description lookup table for captions for the UCOD, POD and mentions models
#'   for the weekly charts
icd_captions <- function(geography = "england") {
  captions <- tibble::tribble(
    ~reference,                                                                                   ~caption,
    "Diabetes mentions",                                                  "ICD10 reference: All mentions of E10-E14",
    "Parkinson's disease mentions",                                                 "ICD10 reference: All mentions of G20",
    "Cirrhosis and other liver diseases mentions",                                             "ICD10 reference: All mentions of K70-K76",
    "Diseases of the urinary system mentions",                                             "ICD10 reference: All mentions of N00-N39",
    "Dementia and Alzheimer's mentions",                                    "ICD10 reference: All mentions of F01, F03, or G30",
    "Other respiratory diseases mentions",       "ICD10 reference: All mentions beginning with J (excluding J00-J22 and J40-J47)",
    "Chronic lower respiratory diseases mentions",                                             "ICD10 reference: All mentions of J40-J47",
    "Acute respiratory infections mentions",                                             "ICD10 reference: All mentions of J00-J22",
    "Cancer mentions",                                             "ICD10 reference: All mentions of C00-C97",
    "Heart failure mentions",                      "ICD10 reference: All mentions of I11.0, I25.5, I42.0, I42.9, I50.0, I50.1, I50.9",
    "Other circulatory diseases mentions",       "ICD10 reference: All mentions beginning with I (excluding I20-I25 and I60-I69)",
    "Cerebrovascular diseases mentions",                                             "ICD10 reference: All mentions of I60-I69",
    "Ischaemic heart diseases mentions",                                             "ICD10 reference: All mentions of I20-I25",
    "Other places POD",                                                                                         NA,
    "Hospice POD",                                                                                         NA,
    "Hospital (acute or community, not psychiatric) POD",                                                                                         NA,
    "Care home (nursing or residential) POD",                                                                                         NA,
    "Home POD",                                                                                         NA)  
  
    return(captions)
}

convert_to_weekly_for_modelling <- function(data, from_date, to_date) {
  # need to make date start on a Monday (Bank Hols, Saturday and Sundays are removed at this point) 
  first_date <- data %>% 
    distinct(date) %>% 
    complete(date = seq(from = from_date,
                        to = to_date,
                        by = "days")) %>% 
    arrange(date) %>% 
    mutate(day_of_week = wday(date, label = TRUE)) %>% 
    filter(day_of_week == "Mon") %>% 
    slice(1) %>% 
    pull(date)
  
  
  
  # need to make date finish on a Friday (Bank Hols, Saturday and Sundays are removed at this point) 
  last_date <- data %>% 
    distinct(date) %>% 
    complete(date = seq(from = from_date,
                        to = to_date,
                        by = "days")) %>% 
    arrange(desc(date)) %>% 
    mutate(day_of_week = wday(date, label = TRUE)) %>% 
    filter(day_of_week == "Fri") %>% 
    slice(1) %>% 
    pull(date)
  
  start_day <- 6 # Saturday = day 1 (if not aligned with ONS, deaths on a Saturday are already included in the Friday)
  
  negate_names <- c("deaths_total", "years_from_20161231",
                    "denominator", "WedpreE", "ThurpreE", "TuespostE",
                    "WedpostE", "ThurpostE", "FripostE", "MonpostE1",
                    "TuespostE1", "BH_nearest_WD", "BH_next_nearest_WD", "month1",           
                    "month2", "month3", "month4", "month5",
                    "month6", "month7", "month8", "month9",
                    "month10", "month11", "month12", "day1",
                    "day2", "day3", "day4", "day5")
  
  missing_names <- setdiff(negate_names, names(data))
  
  # if month fields are missing in build_recent_dates because the recent dates have 
  # yet to be predicted for certain months, then impute them with a 0
  if (length(missing_names) > 0) data[, missing_names] <- 0
  
  weekly <- data %>% 
    filter(between(date, first_date, last_date)) %>% 
    mutate(date = date + (7 - wday(date, week_start = start_day)), # this converts all dates in the week to the Friday date of that week
           fri_xmas = day(date) == 25 & month(date) == 12,
           week_after_fri_xmas = day(date - 7) == 25 & month(date - 7) == 12,
           week2_after_fri_xmas = day(date - 14) == 25 & month(date - 14) == 12) %>% 
    group_by(across(!all_of(c(negate_names, 
                              "fri_xmas", "week_after_fri_xmas", "week2_after_fri_xmas")))) %>% 
    summarise(deaths_total = sum(deaths_total),
              denominator = sum(denominator),
              years_from_20161231 = mean(years_from_20161231),
              month1 = mean(month1),
              month2 = mean(month2),
              month3 = mean(month3),
              month4 = mean(month4),
              month5 = mean(month5),
              month6 = mean(month6),
              month7 = mean(month7),
              month8 = mean(month8),
              month9 = mean(month9),
              month10 = mean(month10),
              month11 = mean(month11),
              month12 = mean(month12),
              easter_pre = max(ThurpreE, na.rm = TRUE),
              easter_post_1 = max(TuespostE, na.rm = TRUE),
              easter_post_2 = max(MonpostE1, na.rm = TRUE),
              wk_nearest_BH = max(BH_nearest_WD, na.rm = TRUE),
              wk_next_nearest_BH = max(BH_next_nearest_WD, na.rm = TRUE),
              wk_fri_xmas = max(fri_xmas, na.rm = TRUE),
              wk_post_fri_xmas = max(week_after_fri_xmas, na.rm = TRUE),
              wk2_post_fri_xmas = max(week2_after_fri_xmas, na.rm = TRUE),
              .groups = "drop")
  
  return(weekly)
}

convert_daily_to_weekly <- function(data, fields_for_grouping, date_field, aggregate_field) {
  start_day <- 6 # Saturday = day 1 (if not aligned with ONS, deaths on a Saturday are already included in the Friday)
  
  weekly <- data %>% 
    complete({{ date_field }} := seq(from = min({{ date_field }}),
                                    to = max({{ date_field }}),
                                    by = "days")) %>% 
    mutate({{ date_field }} := {{ date_field }} + (7 - wday({{ date_field }}, week_start = start_day))) %>% 
    group_by(across(all_of(fields_for_grouping))) %>% 
    filter(n() == 7) %>% 
    summarise({{ aggregate_field }} := sum({{ aggregate_field }}, na.rm = TRUE),
              .groups = "drop")
  
  return(weekly)
}

capitalise_first_letter <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


# consistent preprocessing steps for data processing prior to visualisations
preprocessing_stages <- function(data, eth_dep, 
                                 all_pod, ethnicity_proportions,
                                 deaths_field, bank_holidays, 
                                 new_deaths_field_name = "registered_count") {
  if (eth_dep == TRUE) {
    arrange_columns <- c("RGN09CD", "Ethnic_Group", 
                         "Deprivation_Quintile", 
                         "Sex", "Age_Group")
    
    data <- data %>%
      arrange(across(all_of(arrange_columns))) %>%
      ethnicity_not_stated_adjustment(proportions = ethnicity_proportions,
                                      ethnicity_field = Ethnic_Group, 
                                      deaths_field = {{ deaths_field }}, 
                                      region_field = RGN09CD, 
                                      age_field = Age_Group,  
                                      sex_field = Sex, 
                                      date_field = Reg_Date) %>%
      sex_change_0_1(Sex) %>%
      weekends_to_nearest_work_day(date_field = Reg_Date, 
                                   agg_field = {{ deaths_field }}) %>%
      bank_hols_to_nearest_work_day(bank_holidays = bank_holidays, 
                                    date_field = Reg_Date, 
                                    agg_field = {{ deaths_field }}) %>%
      rename(date = Reg_Date,
             !! new_deaths_field_name := {{ deaths_field }}) %>%
      mutate(Age_Group = factor(Age_Group),
             Ethnic_Group = factor(Ethnic_Group),
             Deprivation_Quintile = factor(Deprivation_Quintile))
    
  } else {
    data <- data %>%
      aggregate_Scillies_CoL(UTLAApr19CD, {{ deaths_field }})
    
    if (all_pod == TRUE) {
      data <- data %>%
        arrange(UTLAApr19CD, POD_out, Sex, Age_Group, Reg_Date)
    } else {
      data <- data %>%
        arrange(UTLAApr19CD, Sex, Age_Group, Reg_Date)
    }
    
    data <- data %>%
      sex_change_0_1(Sex) %>%
      weekends_to_nearest_work_day(date_field = Reg_Date, 
                                   agg_field = {{ deaths_field }}) %>%
      bank_hols_to_nearest_work_day(bank_holidays = bank_holidays, 
                                    date_field = Reg_Date, 
                                    agg_field = {{ deaths_field }}) %>%
      rename(date = Reg_Date,
             !! new_deaths_field_name := {{ deaths_field }}) %>%
      mutate(Age_Group = factor(Age_Group))
  }
  
  return(data)
}

# Write the powerbi collated data to an excel spreadsheet with formatting and 
# links. This isn't the prettiest code, but it should be reasonably robust
write_data_server <- function(geography = c("england", "region"), data, folder_path){
  
  geography <- match.arg(geography)
  
  # data formatting based on whether national or regional
  if (geography == "england") {
    data_sheet <- "Excess mortality England data"
    data_sheet_link <- "National report data"
    contents_path <- "data/excel_contents.csv"
    output_path <- paste0(folder_path, "/weekly_chart_data")
    
    col_order <- quos(
      Chart_Name,
      Region_Code, RGN09NM,
      Week_End,
      Age_Group,
      Sex,
      Deprivation_Quintile,
      Ethnic_Group,
      Cause_Name,
      Place_of_Death,
      Mention_of_Covid,
      Excess_Deaths,
      ratio
    )
    
    new_names <- quos(
      "Region Name" = RGN09NM,
      "Cause of Death" = Cause_Name,
      "Number of deaths with a mention of COVID-19 on death certificate" = Mention_of_Covid,
      "Number of Excess Deaths" = Excess_Deaths,
      "Ratio of registered deaths to expected deaths" = ratio
    )
    
    join_columns <- c("Region_Code" = "RGN09CD")
    
    # UTLA lookup
    geo_lookup <- utla_lookup() %>%
      distinct(RGN09CD, RGN09NM)
    
  } else {
    data_sheet <- "Excess mortality regional data"
    data_sheet_link <- "Regional report data"
    contents_path <- "data/excel_contents_regions.csv"
    output_path <- paste0(folder_path, "/region_weekly_chart_data")
    
    col_order <- quos(
      Chart_Name,
      Region_Code, RGN09NM,
      Period_Start, Period_End,
      Age_Group,
      Sex,
      Deprivation_Quintile,
      Ethnic_Group,
      UTLAApr19CD, UTLAApr19NM,
      Cause_Name,
      Place_of_Death,
      Mention_of_Covid,
      Underlying_cause_covid, Underlying_cause_disease,
      Excess_Deaths,
      ratio
    )
    
    new_names <- quos(
      "Upper Tier Local Authority Code" = UTLAApr19CD,
      "Upper Tier Local Authority Name" = UTLAApr19NM,
      "Region Name" = RGN09NM,
      "Cause of Death" = Cause_Name,
      "Number of deaths with a mention of COVID-19 on death certificate" = Mention_of_Covid,
      "Number of Excess Deaths" = Excess_Deaths,
      "Ratio of registered deaths to expected deaths" = ratio
    )
    
    join_columns <- c("Region_Code" = "RGN09CD", "UTLAApr19CD")
    
    # UTLA lookup
    geo_lookup <- utla_lookup() %>%
      # sneakily adding rows of just region data with NA UTLA data. Allows one join 
      # to get everything we need
      bind_rows(., distinct(., RGN09CD, RGN09NM))
  }
  
  # format data with data from above
  data <- data %>%
    left_join(
      geo_lookup, 
      by = join_columns
    ) %>%
    # reorder columns
    select(!!! col_order) %>%
    mutate(
      Cause_Name = case_when(
        Chart_Name == "Mention of cause of death" & Cause_Name == "ARI mentions" ~ "Mentions of Acute Respiratory Infections",
        Chart_Name == "Mention of cause of death" & Cause_Name == "Dementia mentions" ~ "Mentions of Dementia & Alzheimer's disease",
        Chart_Name == "Mention of cause of death" & Cause_Name == "Diabetes mentions" ~ "Mentions of Diabetes",
        TRUE ~ Cause_Name
      )
    )
  
  # use new_names for excel output
  # csv will just use 'data' (with old names with underscores)
  data_excel <- data %>%
    # change names around
    rename(!!! new_names) %>%
    # take out underscores in names
    rename_with(
      ~ gsub("_", " ", .x)
    )
  
  # Workbook
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = "Contents")
  openxlsx::addWorksheet(wb, sheetName = data_sheet)
  
  # Data Sheet
  # add data to data_sheet
  openxlsx::writeDataTable(
    wb, sheet = data_sheet, x = data_excel,
    colNames = TRUE, rowNames = FALSE,
    keepNA = TRUE, na.string = "NA",
    tableStyle = "none",
    withFilter = FALSE,
    headerStyle = openxlsx::createStyle(textDecoration = "bold", wrapText = TRUE)
  )
  # format data_sheet
  openxlsx::freezePane(wb, sheet = 2, firstRow = TRUE, firstCol = FALSE)
  openxlsx::setColWidths(wb, sheet = 2, cols = 1:nrow(data), widths = "auto")
  
  # Contents page
  # read in contents
  contents <- read_csv(
    contents_path,
    na = character(),
    col_types = cols(
      text = col_character(),
      class = col_character(),
      x = col_character(),
      y = col_double(),
      style = col_character(),
      font_size = col_double()
    )
  ) %>% 
    # add in the date of the latest report
    mutate(
      text = case_when(
        # add the date of the Thursday of the current week
        # e.g. ran on any day will return the date of the thursday between the 
        # last sunday and next saturday
        text == "date" ~ format(Sys.Date() - lubridate::wday(Sys.Date()) + 5, "%d %B %Y"),
        TRUE ~ text
      )
    )
  
  # Add in link to data_sheet
  contents_row <- contents[contents$text == "Contents:", ]
  # add style before contents, otherwise style seems to get lost
  openxlsx::addStyle(
    wb, sheet = "Contents",
    rows = contents_row$y + 1, 
    cols = contents_row$x,
    style = openxlsx::createStyle(
      textDecoration = contents_row$style,
      fontSize = contents_row$font_size
    )
  )
  
  openxlsx::writeFormula(
    wb, sheet = "Contents",
    # will go in the cell below the "Contents:"
    startRow = contents_row$y + 1,
    startCol = contents_row$x,
    x = openxlsx::makeHyperlinkString(
      sheet = data_sheet, row = 1, col = 1, text = data_sheet_link
    )
  )
  
  # function to help us write into cells with formatting
  write_cell <- function(workbook, sheet, text, x, y, class, style, font_size){
    
    class(text) <- class
    # as above: add style before contents, otherwise style seems to get lost
    openxlsx::addStyle(
      workbook, sheet = sheet,
      cols = x, rows = y,
      style = openxlsx::createStyle(textDecoration = style, fontSize = font_size)
    )
    
    openxlsx::writeData(
      workbook, sheet = sheet, 
      x = text, xy = c(x, y),
      colNames = FALSE, rowNames = FALSE
    )
  }
  
  # walk through our contents data and  write into cells
  pwalk(
    .l = contents, 
    .f = write_cell,
    workbook = wb,
    sheet = "Contents"
  )
  
  # Save to excel
  openxlsx::saveWorkbook(wb, file = paste0(output_path, ".xlsx"), overwrite = TRUE)
  # Save .csv
  readr::write_csv(data, paste0(output_path, ".csv"))
  # Return output_path
  output_path
}

# return length of icd codes. If lengths are different error out.
icd_codes_length <- function(codes){
  
  nchars <- unique(nchar(codes))
  
  if (length(nchars) > 1){
    
    stop("Need consistent length ICD codes")
    
  }
  
  nchars
  
}

# removes columns in a data frame that are only NA or "All"
remove_columns_with_all_or_nas_only <- function(data) {
  
  data_process <- data %>% 
    dplyr::select(`Chart Group` = type,
                  `Population Group` = Chart_Name,
                  `Population Subgroup` = Plot_Label,
                  `Week Ending` = date,
                  `Area Code` = RGN09CD,
                  `Area Name` = RGN09NM,
                  Sex,
                  `Ethnic Group` = Ethnic_Group,
                  `Age Group` = Age_Group,
                  `Cause of Death` = name_of_cause,
                  `Place of Death` = POD_out,
                  `Deprivation Quintile` = Deprivation_Quintile,
                  `Registered Deaths` = all_dths,
                  `Expected Deaths` = exptd_dths,
                  `Deaths with COVID-19 on the Death Certificate` = covid_dths,
                  `Deaths with COVID-19 as the Underlying Cause` = ucod_covid,
                  `Deaths with Specific Disease as the Underlying Cause` = ucod_disease)
  
  number_of_records <- nrow(data_process)
  remove_cols <- (colSums(data_process == "All") == number_of_records |
                    colSums(is.na(data_process)) == number_of_records)
  
  remove_cols <- replace(remove_cols, is.na(remove_cols), FALSE)
  
  data_process <- data_process[, !remove_cols]
  
  data_process <- data_process %>% 
    mutate(`Excess Deaths` = `Registered Deaths` - `Expected Deaths`,
           across(any_of(c("Registered Deaths", "Expected Deaths", 
                           "Excess Deaths", "Deaths with COVID-19 on the Death Certificate")), 
                  ~round_correct(.x, n = 0))) %>% 
    relocate(`Excess Deaths`, .after = `Expected Deaths`) %>% 
    relocate(where(is.numeric), .after = last_col())
  
  return(data_process)
}

add_data_to_excel <- function(data, wb, output_filepath) {
  data_sheet <- as.character(unique(data$`Chart Group`))
  
  # Data Sheet
  # add data to data_sheet
  # write_ods(
  #   x = data,
  #   path = output_filepath,
  #   sheet = data_sheet,
  #   update = TRUE,
  #   append = TRUE
  # )
  openxlsx::addWorksheet(wb, sheetName = data_sheet)
  openxlsx::writeDataTable(
    wb, 
    sheet = data_sheet, 
    x = data,
    colNames = TRUE, rowNames = FALSE,
    keepNA = TRUE, na.string = "NA",
    tableStyle = "none",
    withFilter = FALSE,
    headerStyle = openxlsx::createStyle(textDecoration = "bold", wrapText = TRUE)
  )
}

create_excel_file <- function(input_filepath, output_filepath, 
                              geography = "england") {
  sheet_references <- read.csv("data/excel_sheet_references.csv") %>% 
    tibble::deframe()
  
  download_data <- read.csv(input_filepath) %>% 
    mutate(type = factor(type,
                         levels = names(sheet_references)))
  
  download_data <- split(download_data, download_data$type)
  
  download_data <- lapply(download_data, 
                          remove_columns_with_all_or_nas_only)
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = "Contents")

  lapply(download_data, add_data_to_excel,
         wb = wb,
         output_filepath = output_filepath)
  
  if (geography == "england") {
    contents_path <- "data/excel_contents.csv"
  } else {
    contents_path <- "data/excel_contents_regions.csv"
  }
  
  contents <- read_csv(
    contents_path,
    na = character(),
    col_types = cols(
      text = col_character(),
      class = col_character(),
      x = col_character(),
      y = col_double(),
      style = col_character(),
      font_size = col_double()
    )
  ) %>% 
    # add in the date of the latest report
    mutate(
      text = case_when(
        # add the date of the Thursday of the current week
        # e.g. ran on any day will return the date of the thursday between the 
        # last sunday and next saturday
        text == "date" ~ format(Sys.Date() - lubridate::wday(Sys.Date()) + 5, "%d %B %Y"),
        TRUE ~ text
      )
    )
  
  # Add in link to data_sheet
  contents_row <- contents[contents$text == "Contents:", ]
  # add style before contents, otherwise style seems to get lost
  
  for (i in seq_along(download_data)) {
    
    openxlsx::addStyle(
      wb, sheet = "Contents",
      rows = contents_row$y + i, 
      cols = contents_row$x,
      style = openxlsx::createStyle(
        textDecoration = contents_row$style,
        fontSize = contents_row$font_size
      )
    )
    
    openxlsx::writeFormula(
      wb, sheet = "Contents",
      # will go in the cell below the "Contents:"
      startRow = contents_row$y + i,
      startCol = contents_row$x,
      x = openxlsx::makeHyperlinkString(
        sheet = names(download_data)[[i]], row = 1, col = 1, 
        text = sheet_references[names(download_data)[[i]]]
      )
    )
  }
  
  
  # function to help us write into cells with formatting
  write_cell <- function(workbook, sheet, text, x, y, class, style, font_size, add_rows, threshold_row){
    
    if (y <= threshold_row) add_rows <- 0
    
    class(text) <- class
    # as above: add style before contents, otherwise style seems to get lost
    openxlsx::addStyle(
      workbook, sheet = sheet,
      cols = x, rows = y + add_rows,
      style = openxlsx::createStyle(textDecoration = style, fontSize = font_size)
    )
    
    openxlsx::writeData(
      workbook, sheet = sheet, 
      x = text, xy = c(x, y + add_rows),
      colNames = FALSE, rowNames = FALSE
    )
  }
  
  # walk through our contents data and  write into cells
  pwalk(
    .l = contents, 
    .f = write_cell,
    workbook = wb,
    sheet = "Contents",
    add_rows = length(download_data),
    threshold_row = contents$y[match("Contents:", contents$text)]
  )
  
  openxlsx::saveWorkbook(wb, file = output_filepath, overwrite = TRUE)
  return(output_filepath)
}

convert_to_ods <- function(excel_filepath) {
  library(RDCOMClient)
  ex <- COMCreate("Excel.Application")
  book <- ex$Workbooks()$Open(excel_filepath)
  
  new_filepath <- gsub("xlsx$", "ods", excel_filepath)
  new_filepath <- gsub("/", "\\\\", new_filepath)
  
  if (file.exists(new_filepath)) file.remove(new_filepath)
  
  book$SaveAS(Filename = new_filepath,
              FileFormat = 60)
  
  book$Close()
  ex$Quit()
  return(new_filepath)
}


qa_power_bi_file <- function(filepath) {
  df <- read.csv(filepath)
  
  compare_cols <- c("all_dths", "exptd_dths", "covid_dths")
  
  
  # check non-ethnic group totals -------------------------------------------
  
  all_person_totals <- df %>% 
    filter(type == "England") %>% 
    summarise(across(all_of(compare_cols),
                     sum),
              .groups = "drop")
  
  total_checks <- df %>% 
    filter(Sex == "Persons",
           RGN09CD == "E92000001",
           !(type %in% c("England", "all cause")),
           across(
             all_of(
               c("Ethnic_Group", "Age_Group", "POD_out", "Deprivation_Quintile")
             ),
             function(x) x %in% c(" All", "All")
           )) %>% 
    group_by(type) %>% 
    summarise(across(all_of(compare_cols),
                     sum),
              .groups = "drop")
  
  total_checks_ethnicity <- total_checks %>% 
    filter(type == "Ethnicity")
  
  total_checks <- total_checks %>% 
    filter(type != "Ethnicity")
  
  subgroup_age_checks <- df %>% 
    filter(type == "Age_Group",
           Age_Group != " All") %>% 
    mutate(Sex = if_else(Sex == "Persons", "By person", "By gender")) %>% 
    group_by(type, Sex) %>% 
    summarise(across(all_of(compare_cols),
                     sum),
              .groups = "drop")
  
  subgroup_deprivation_checks <- df %>% 
    filter(type == "deprivation",
           Deprivation_Quintile != "All") %>% 
    group_by(type) %>% 
    summarise(across(all_of(compare_cols),
                     sum),
              .groups = "drop")
  
  
  subgroup_pod_checks <- df %>% 
    filter(type == "pod",
           POD_out != "All") %>% 
    group_by(type) %>% 
    summarise(across(all_of(compare_cols),
                     sum),
              .groups = "drop")
  
  subgroup_region_checks <- df %>% 
    filter(type == "region",
           RGN09CD != "E92000001") %>% 
    group_by(type) %>% 
    summarise(across(all_of(compare_cols),
                     sum),
              .groups = "drop")
  
  subgroup_utla_checks <- df %>% 
    filter(type == "utla") %>% 
    group_by(type) %>% 
    summarise(across(all_of(compare_cols),
                     sum),
              .groups = "drop")
  
  non_ethnicity_combined <- bind_rows(
    total_checks,
    subgroup_age_checks,
    subgroup_deprivation_checks,
    subgroup_pod_checks,
    subgroup_region_checks,
    subgroup_utla_checks
  )
  
  for (col in compare_cols) {
    non_ethnicity_combined[[col]] <- non_ethnicity_combined[[col]] - all_person_totals[[col]]
  }
  
  # check ethnic group totals -----------------------------------------------
  
  all_person_ethnicity_totals <- df %>% 
    mutate(date = as.Date(date)) %>% 
    filter(type == "England", 
           date != max(date)) %>% 
    summarise(across(all_of(compare_cols),
                     sum),
              .groups = "drop")
  
  
  subgroup_ethnicity_checks <- df %>% 
    filter(type == "Ethnicity",
           Ethnic_Group != "All") %>% 
    mutate(Sex = if_else(Sex == "Persons", "By person", "By gender")) %>% 
    group_by(type, Sex) %>% 
    summarise(across(all_of(compare_cols),
                     sum),
              .groups = "drop")
  
  ethnicity_combined <- bind_rows(
    total_checks_ethnicity,
    subgroup_ethnicity_checks
    
  )
  
  for (col in compare_cols) {
    ethnicity_combined[[col]] <- ethnicity_combined[[col]] - all_person_ethnicity_totals[[col]]
  }
  
  all_comparisons <- bind_rows(
    non_ethnicity_combined,
    ethnicity_combined
  ) %>% 
    mutate(across(all_of(compare_cols),
                  ~ round_correct(.x, n = 4)))
  return(all_comparisons)
}

date_as_string <- function(path, 
                           week_type = c("this week", "last week"),
                           date_type = c("report end date", "publication date")) {
  
  week_type <- match.arg(week_type)
  date_type <- match.arg(date_type)
  
  current_date <- tools::file_path_sans_ext(path) %>% 
    basename() %>% 
    stringr::str_extract("[0-9]+") %>% 
    as.Date("%Y%m%d")
  
  if (date_type == "publication date") current_date <- current_date + 13
  
  if (week_type == "this week") {
    date_as_string <- as.character(current_date) %>% 
      stringr::str_replace_all("-", "")
  } else if (week_type == "last week") {
    date_as_string <- as.character(current_date - 7) %>% 
      stringr::str_replace_all("-", "")
  }
  return(date_as_string)
}

compare_this_and_last_weeks_file <- function(path) {
  
  old_path <- str_replace(path, 
                          date_as_string(path, week_type = "this week"),
                          date_as_string(path, week_type = "last week"))
  
  
  # calculate the totals for each subgroup ----------------------------------

  compare_cols <- c(registered = "all_dths", 
                    expected = "exptd_dths", 
                    covid_mentions = "covid_dths", 
                    underlying_specific_cause = "ucod_disease", 
                    underlying_covid = "ucod_covid")

  unnamed_compare_cols <- unname(compare_cols)
  
  new_file_totals <- read.csv(path) %>% 
    group_by(type, Plot_Label, Sex) %>% 
    filter(date != max(date)) %>% 
    summarise(across(all_of(unnamed_compare_cols),
                     sum),
              .groups = "drop") %>% 
    rename_with(~ paste0(.x, ".new"), all_of(unnamed_compare_cols))
  
  old_file_totals <- read.csv(old_path) %>% 
    group_by(type, Plot_Label, Sex) %>% 
    summarise(across(all_of(unnamed_compare_cols),
                     sum),
              .groups = "drop") %>% 
    rename_with(~ paste0(.x, ".old"), all_of(unnamed_compare_cols))
  
  data_compared <- new_file_totals %>% 
    left_join(old_file_totals, by = c("type", "Plot_Label", "Sex")) %>% 
    tidyr::pivot_longer(cols = !c(type, Plot_Label, Sex),
                        names_to = "death_type",
                        values_to = "count") %>% 
    tidyr::separate(death_type, 
                    into = c("death_type", "file_type"),
                    sep = "\\.") %>% 
    tidyr::pivot_wider(names_from = file_type,
                       values_from = count) %>% 
    mutate(across(all_of(c("new", "old")),
                  function(x) if_else(is.na(x), 0, x)),
           new_minus_old = new - old,
           new_minus_old = round_correct(new_minus_old, 1)) %>% 
    dplyr::select(!c(new, old)) %>% 
    arrange(desc(abs(new_minus_old))) %>% 
    left_join(tibble::enframe(compare_cols,
                              name = "count_type",
                              value = "death_type"),
              by = "death_type") %>% 
    dplyr::select(type, Plot_Label, Sex, count_type, new_minus_old)
  
  return(data_compared)
}


aggregate_regional_to_national <- function(regional_data_filepath) {
  df_region <- read.csv(regional_data_filepath)
  
  # this section recreates the "region" type in the national dataframe 
  # from the region dataframe
  
  region_subset <- df_region %>% 
    filter(type == "England") %>% 
    mutate(type = "region",
           RGN09NM = case_when(
             RGN09NM == "Yorkshire and The Humber" ~ "Yorkshire & Humber",
             TRUE ~ RGN09NM
           ),
           Chart_Name = "Region")
  
  df_region_regions <- region_subset %>% 
    group_by(date, Dispersion_Parameter, days_in_week) %>% 
    summarise(across(c(all_dths, exptd_dths, covid_dths,
                       cuml_covid_dths, cuml_all_dths, cuml_exptd_dths,
                       ucod_covid, ucod_disease),
                     sum),
              .groups = "drop") %>% 
    mutate(excess_deficit = case_when(
      all_dths < exptd_dths ~ all_dths - exptd_dths, 
      TRUE ~ as.numeric(0)),
      tot_excess = case_when(
        all_dths > exptd_dths ~ all_dths - exptd_dths,
        TRUE ~ as.numeric(0)
      ),
      type = "region",
      RGN09CD = "E92000001",
      RGN09NM = "England",
      Sex = "Persons", 
      Ethnic_Group = "All", 
      Age_Group = "All", 
      name_of_cause = "All", 
      POD_out = "All", 
      Deprivation_Quintile = "All",
      Plot_Label = RGN09NM,
      Chart_Name = "Region"
    ) %>% 
    bind_rows(region_subset) %>% 
    mutate(RGN09NM = gsub("England", " England", RGN09NM), 
           Plot_Label = RGN09NM)
  
  # this section aggregates all the regional data to england
  
  region_aggregated <- df_region %>% 
    filter(type != "utla") %>% 
    group_by(type, Sex, Ethnic_Group,
             Age_Group, name_of_cause, POD_out, Deprivation_Quintile,
             date, days_in_week, Plot_Label, Chart_Name, Dispersion_Parameter) %>% 
    summarise(across(c(all_dths, exptd_dths, covid_dths,
                       cuml_covid_dths, cuml_all_dths, cuml_exptd_dths,
                       excess_deficit, tot_excess,
                       ucod_covid, ucod_disease),
                     sum),
              .groups = "drop") %>% 
    mutate(
      excess_deficit = case_when(
        all_dths < exptd_dths ~ all_dths - exptd_dths,
        TRUE ~ as.numeric(0)),
      tot_excess = case_when(
        all_dths > exptd_dths ~ all_dths - exptd_dths,
        TRUE ~ as.numeric(0)
      ),
      RGN09CD = "E92000001",
      RGN09NM = "England",
    ) %>% 
    bind_rows(df_region_regions,
              df_region %>% 
                filter(type == "utla"))
  
  return(region_aggregated)
}

