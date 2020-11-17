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
    group_by_at(vars(- {{ agg_field }})) %>%
    summarise(!!as_label(enquo(agg_field)) := sum({{ agg_field }}),
              .groups = "keep") %>%
    ungroup()
  return(data)
}

#' @description returns a vector of easter fridays when given a vector of holiday dates
calc_easter_fridays <- function(dates) {
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
           )) %>%
    filter(easter_friday,
           month != 12) %>%
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
    mutate(saturday = wday({{ date_field }}) == 7,
           saturday_mark = case_when(saturday == TRUE | lead(saturday == TRUE) ~ TRUE,
                                     TRUE ~ FALSE),
           sunday = wday({{ date_field }}) == 1,
           sunday_mark = case_when(sunday == TRUE | lag(sunday == TRUE) ~ TRUE,
                                   TRUE ~ FALSE))
  
  friday_data <- weekend_data %>%
    filter(saturday_mark == TRUE) %>%
    mutate(group_id = cumsum(1 - saturday)) %>%
    group_by_at(vars(- {{ agg_field }}, 
                     - {{ date_field }},
                     -saturday_mark,
                     -saturday,
                     -sunday_mark,
                     -sunday)) %>%
    summarise({{ date_field }} := min({{ date_field }}),
              aggregated_saturday_data = sum({{ agg_field }}),
              .groups = "keep") %>%
    ungroup() %>%
    dplyr::select(-group_id)
  
  monday_data <- weekend_data %>%
    mutate(group_id = cumsum(sunday)) %>%
    filter(sunday_mark == TRUE) %>%
    group_by_at(vars(- {{ agg_field }}, 
                     - {{ date_field }},
                     -saturday_mark,
                     -saturday,
                     -sunday_mark,
                     -sunday)) %>%
    summarise({{ date_field }} := max({{ date_field }}),
              aggregated_sunday_data = sum({{ agg_field }}),
              .groups = "keep") %>%
    ungroup() %>%
    dplyr::select(-group_id)
  
  data <- data %>%
    left_join(friday_data, 
              by = intersect(names(.), names(friday_data))) %>%
    left_join(monday_data, 
              by = intersect(names(.), names(monday_data))) %>%
    mutate({{ agg_field }} := case_when(wday({{ date_field }}) %in% c(1, 7) ~ 0,
                                        !is.na(aggregated_saturday_data) ~ aggregated_saturday_data,
                                        !is.na(aggregated_sunday_data) ~ aggregated_sunday_data,
                                        TRUE ~ {{ agg_field }})) %>%
    dplyr::select(-aggregated_saturday_data, -aggregated_sunday_data)
  
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
              .groups = "keep") %>%
    ungroup() %>%
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
              .groups = "keep") %>%
    ungroup() %>%
    dplyr::select(-group_id)
  
  any_non_easter_bhols <- any(non_easter_holidays %in% pull(data, {{ date_field }}))
  if (any_non_easter_bhols) {
    friday_bhols <- non_easter_holidays[wday(non_easter_holidays) == 6 & 
                                          month(non_easter_holidays) != 12]
    monday_bhols <- non_easter_holidays[wday(non_easter_holidays) == 2 & 
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
                  .groups = "keep") %>%
        ungroup() %>%
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
                  .groups = "keep") %>%
        ungroup() %>%
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
                .groups = "keep") %>%
      ungroup() %>%
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
    weekends_to_nearest_work_day(date_field = {{ date_field }}, agg_field = {{ death_field }}) %>%
    bank_hols_to_nearest_work_day(bank_holidays = holidays, 
                                  date_field = {{ date_field }},
                                  agg_field = {{ death_field }}) %>%
    filter(!(wday({{ date_field }}) %in% c(1, 7)),
           !({{ date_field }} %in% holidays),
           Sex != 3) %>%
    mutate(day = wday({{ date_field }}), 
           # year = year({{ date_field }}), 
           ### add binary variables for days either side of Easter
           month = floor_date({{ date_field }}, unit = "month"), 
           years_from_20161231 = as.numeric({{ date_field }} - as.Date("2016-12-31")) / 365.25) %>%
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
build_recent_dates <- function(area_codes, from_date, to_date, holidays, denominators, utla_lkp, ethnicity, deprivation, 
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
  
  if (ethnicity == TRUE | deprivation == TRUE) {
    recent_dates <- recent_dates %>%
      merge(y = data.frame(RGN09CD = area_codes, stringsAsFactors = FALSE))
    if (ethnicity == TRUE) {
      recent_dates <- recent_dates %>%
        merge(y = data.frame(Ethnic_Group = factor(c("Asian", "Black", "Mixed", "Other", "White"))))  
    }
    
    if (deprivation == TRUE){
      recent_dates <- recent_dates %>%
        merge(y = data.frame(Deprivation_Quintile = factor(1:5)))
    } 
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
  
  if (ethnicity == TRUE) {
    if (deprivation == TRUE) {
      recent_dates <- recent_dates %>%
        left_join(denominators, by = c("RGN09CD" = "OfficialCode", "Ethnic_Group", "Deprivation_Quintile", "Sex", "Age_Group", "month"))  
    } else {
      recent_dates <- recent_dates %>%
        left_join(denominators, by = c("RGN09CD" = "OfficialCode", "Ethnic_Group", "Sex", "Age_Group", "month"))  
    }
  } else {
    if (deprivation == TRUE) {
      recent_dates <- recent_dates %>%
        left_join(denominators, by = c("RGN09CD" = "OfficialCode", "Deprivation_Quintile", "Sex", "Age_Group", "month"))
    } else if (deprivation == FALSE) {
      recent_dates <- recent_dates %>%
        left_join(denominators, by = c("UTLAApr19CD" = "OfficialCode", "Sex", "Age_Group", "month"))
    }
    
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
  
  ## add zeros in missing fields (ie, because the month hasn't occurred in the recent_dates object)
  if (ethnicity == TRUE) {
    if (deprivation == TRUE) {
      expected_names <- c("date", "RGN09CD", "Ethnic_Group", "Deprivation_Quintile", "Sex", 
                          "Age_Group", "deaths_total", 
                          "years_from_20161231", "denominator", #"year", 
                          "WedpreE", "ThurpreE", "TuespostE", 
                          "WedpostE", "ThurpostE", "FripostE", "MonpostE1",
                          "TuespostE1", "BH_nearest_WD", "BH_next_nearest_WD",
                          paste0("month", 1:12), 
                          paste0("day", 1:5))
    } else {
      expected_names <- c("date", "RGN09CD", "Ethnic_Group", "Sex", 
                          "Age_Group", "deaths_total", 
                          "years_from_20161231", "denominator", #"year", 
                          "WedpreE", "ThurpreE", "TuespostE", 
                          "WedpostE", "ThurpostE", "FripostE", "MonpostE1",
                          "TuespostE1", "BH_nearest_WD", "BH_next_nearest_WD",
                          paste0("month", 1:12), 
                          paste0("day", 1:5))
      
    }
  } else {
    if (deprivation == TRUE) {
      expected_names <- c("date", "RGN09CD", "Deprivation_Quintile", "Sex", 
                          "Age_Group", "deaths_total",  
                          "years_from_20161231", "denominator", #"year", 
                          "WedpreE", "ThurpreE", "TuespostE", 
                          "WedpostE", "ThurpostE", "FripostE", "MonpostE1",
                          "TuespostE1", "BH_nearest_WD", "BH_next_nearest_WD",
                          paste0("month", 1:12), 
                          paste0("day", 1:5))
    } else if (deprivation == FALSE) {
      expected_names <- c("date", "UTLAApr19CD", "Sex", 
                          "Age_Group", "deaths_total",  
                          "years_from_20161231", "denominator", #"year", 
                          "WedpreE", "ThurpreE", "TuespostE", 
                          "WedpostE", "ThurpostE", "FripostE", "MonpostE1",
                          "TuespostE1", "BH_nearest_WD", "BH_next_nearest_WD",
                          paste0("month", 1:12), 
                          paste0("day", 1:5))
    }
    
  }
  
  
  missing_fields <- setdiff(expected_names,
                            names(recent_dates))
  
  recent_dates[missing_fields] <- 0
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

#' @description apply comparability ratios to different disease groups
#' @param data data from that includes a deaths field
#' @param disease_name string; the disease name used in the comparability ratios
#'   Excel lookup table
apply_comparability_ratios <- function(data, disease_name) {
  age_group_expand <- age_group_lkp() %>%
    mutate(Agegroup = case_when(
      Age < 75 ~ "<75",
      TRUE ~ "75+"
    )) %>%
    distinct(Age_Group, Agegroup)

  comparability_ratios <- read_xlsx("data/MUSE comparability ratios v2.xlsx",
                                    sheet = "Final",
                                    skip = 2) %>%
    filter(`Cause of death` == disease_name) %>%
    left_join(age_group_expand, by = "Agegroup") %>%
    dplyr::select(-Agegroup, -`Cause of death`)
  
  data <- data %>%
    left_join(comparability_ratios, by = c("Age_Group", "Sex")) %>%
    mutate(deaths_total = deaths_total * `Comparability ratio`) %>%
    dplyr::select(-`Comparability ratio`)
  
  return(data)
  
  
}

#' @description a cause code to disease lookup table
#' @param cause_name character vector to filter output with
#' @param table_output logical; FALSE returns vector of ICD codes, TRUE returns
#'   a table with cause_name and ICD codes
cause_code_lookup <- function(cause_name = NULL, table_output = FALSE) {
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
             ICD_codes %in% covid_codes ~ "COVID-19",
             TRUE ~ "All other causes (excl. COVID-19)"
           ))
  
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

#' @description lookup table for models used and diseases/place of death to
#'   ensure consistency in predictions and visualisations at England and
#'   regional level
model_references <- function() {
  references <- tibble::tribble(
    ~reference,                                                   ~model_file,                                                                                   ~caption,
    NA,                                                        "model_outputs\\glm_all_utlas_dm_20200522.rds",                                                 "ICD10 reference: All mentions of E10-E14",
    NA,                                                        "model_outputs\\glm_all_utlas_ari_mentions_20200522.rds",                                       "ICD10 reference: All mentions of J00-J22",
    NA,                                                        "model_outputs\\glm_all_utlas_dementia_mentions_20200522.rds",                                  "ICD10 reference: All mentions of F01, F03 and G30",
    "All other causes (excl. COVID-19)",                       "model_outputs\\glm_all_utlas_all_other_20200522.rds",                                          "ICD10 reference: All other underlying causes of death (excluding COVID-19)",
    "Parkinson's disease",                                     "model_outputs\\20200626_parkinsons.rds",                                         "ICD10 reference: Underlying cause of death is G20",
    "cirrhosis and other liver diseases",                      "model_outputs\\20200626_cirrhosis_liver.rds",                                    "ICD10 reference: Underlying cause of death is between K70-K76",
    "diseases of the urinary system",                          "model_outputs\\20200626_urinary_system.rds",                                     "ICD10 reference: Underlying cause of death is between N00-N39",
    "dementia and Alzheimer's",                                "model_outputs\\glm_all_utlas_dementia_20200522.rds",                                           "ICD10 reference: Underlying cause of death is F01, F03 or G30",
    "other respiratory diseases",                              "model_outputs\\20200626_other_respiratory.rds",                                  "ICD10 reference: Underlying cause of death begins with J (excluding J00-J22 and J40-J47)",
    "chronic lower respiratory diseases",                      "model_outputs\\20200626_clr.rds",                                                "ICD10 reference: Underlying cause of death J40-J47",
    "acute respiratory infections",                            "model_outputs\\20200626_ari.rds",                                                "ICD10 reference: Underlying cause of death J00-J22",
    "cancer",                                                  "model_outputs\\glm_all_utlas_cancer_20200522.rds",                                             "ICD10 reference: Underlying cause of death C00-C97",
    "other circulatory diseases",                              "model_outputs\\glm_all_utlas_other_circulatory_20200522.rds",                                  "ICD10 reference: Underlying cause of death begins with I (excluding I20-I25 and I60-I69)",
    "cerebrovascular diseases",                                "model_outputs\\20200626_cerebrovascular.rds",                                              "ICD10 reference: Underlying cause of death I60-I69",
    "ischaemic heart diseases",                                "model_outputs\\20200626_ihd.rds",                                                "ICD10 reference: Underlying cause of death I20-I25",
    "Other places",                                            "model_outputs\\glm_all_utlas_other_places_20200522.rds",                                       "",
    "Hospice",                                                 "model_outputs\\glm_all_utlas_hospice_20200522.rds",                                            "",
    "Hospital (acute or community, not psychiatric)",          "model_outputs\\glm_all_utlas_hospital_20200522.rds",                                           "",
    "Care home (nursing or residential)",                      "model_outputs\\glm_all_utlas_care_home_20200522.rds",                                          "",
    "Home",                                                    "model_outputs\\glm_all_utlas_home_20200522.rds",                                               ""
  )
  
  
  return(references)
}

#' @description check to ensure visualisation_geography used is within range for
#'   given combination of ethnicity and deprivation (as models are built at
#'   different geographies)
#' @inheritParams generate_visualisations
vis_geography_variable <- function(visualisation_geography, ethnicity, deprivation) {
  if (ethnicity == FALSE & deprivation == FALSE) {
    if (!(visualisation_geography %in% c("region", "england"))) stop("visualisation_geography must be either region, england")
  } else {
    if (!(visualisation_geography %in% c("utla", "region", "england"))) stop("visualisation_geography must be either utla, region, england")
  }
  
  if (visualisation_geography == "utla") {
    return("UTLAApr19CD")
  } else if (visualisation_geography == "region") {
    return("RGN09CD") # currently deprivation is RGN09CD and ethnicity is RGN09NM
  } else {
    return(NULL)
  }
}

#' @description check to ensure facet_variables entered is within possible range
#' @inheritParams generate_visualisations
facet_variables <- function(facet_fields = NULL, ethnicity, deprivation) {
  if (is.null(facet_fields)) return(NULL)
  
  # if (!(names(facet_fields) %in% c("x", "y"))) stop("names of facet_fields must be x or y")
  
  if ("Ethnic_Group" %in% facet_fields & ethnicity == FALSE) 
    stop("ethnicity must be equal to TRUE if Ethnic_Group one of facet_fields items")
  
  if ("Deprivation_Quintile" %in% facet_fields & deprivation == FALSE) 
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
export_powerbi_data <- function(data, grouping_fields, model_filename, end_date,
                                directory = Sys.getenv("POWERBI_FILESHARE"), age_filter = NULL) {
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
  
  all_dates <- tibble(date = seq(from = min(data$date),
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
  
  data <- data %>% 
    full_join(all_dates, by = intersect(names(.), names(all_dates))) %>% 
    arrange(across(all_of(grouping_fields))) %>% 
    mutate(sun = wday(date) == 1,
           weekid = cumsum(sun)) %>% 
    ungroup() %>% 
    group_by(across(all_of(c(grouping_fields_without_date_with_weekid, "weekid")))) %>% 
    summarise(date = max(date),
              registered = sum(registered_count, na.rm = TRUE),
              expected = sum(modelled_deaths_zeros, na.rm = TRUE),
              covid = sum(deaths_total, na.rm = TRUE),
              days_in_week = n(),
              .groups = "keep") %>% 
    ungroup() %>% 
    filter(date <= end_date) %>% 
    dplyr::select(-weekid) %>% 
    mutate(date = date - 1)
  
  
  
  if (!is.null(age_filter)) {
    filename <- paste0(directory, "/", model_filename,
                       "_",
                       paste(grouping_fields_without_date, collapse = "_"),
                       "_",
                       paste(age_filter, collapse = "_"),
                       ".csv")
  } else {
    filename <- paste0(directory, "/", model_filename,
                       paste(grouping_fields_without_date, collapse = "_"),
                       ".csv")
  }
  
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


cabinet_office_data_feed <- function() {
  folder_path <- Sys.getenv("POWERBI_FILESHARE")
  
  ucods <- read.csv(paste0(folder_path, "/name_of_cause.csv")) %>% 
    mutate(underlying_or_mention = "underlying")
  
  dm <- read.csv(paste0(folder_path, "/glm_all_utlas_dm_20200522.csv")) %>% 
    mutate(name_of_cause = "diabetes mellitus",
           underlying_or_mention = "mention")
  ari <- read.csv(paste0(folder_path, "/glm_all_utlas_ari_mentions_20200522.csv")) %>% 
    mutate(name_of_cause = "acute respiratory infections",
           underlying_or_mention = "mention")
  dm <- read.csv(paste0(folder_path, "/glm_all_utlas_dementia_mentions_20200522.csv")) %>% 
    mutate(name_of_cause = "dementia",
           underlying_or_mention = "mention")
  
  final <- bind_rows(ucods,
                     dm, 
                     ari,
                     dm) %>% 
    mutate(name_of_cause = tools::toTitleCase(name_of_cause)) %>% 
    rename(week_ending = date) %>% 
    dplyr::relocate(underlying_or_mention) %>% 
    filter(days_in_week == 7) %>% 
    dplyr::select(!c(covid, days_in_week))
  
  filepath <- paste0(folder_path, "/causes_of_death_for_cabinet_office.csv")
  write.csv(final,
            filepath,
            row.names = FALSE)
  
  return(filepath)
}

collate_powerbi_files <- function(remove_expected_registered = FALSE) {
  folder_path <- Sys.getenv("POWERBI_FILESHARE")
  
  file_references <- read.csv("data/powerbi_references.csv") %>% 
    mutate(powerbi_file = paste0(folder_path, "/", powerbi_file, ".csv")) %>% 
    tibble::deframe()
  
  collated_output <- lapply(file_references, read.csv) %>% 
    bind_rows(.id = "Chart_Name") %>% 
    filter(days_in_week == 7) %>% 
    mutate(excess_deaths = round_correct(registered - expected, 0),
           ratio = round_correct(registered / expected, 2),
           name_of_cause = case_when(
             grepl("mentions", Chart_Name) ~ Chart_Name,
             TRUE ~ name_of_cause),
           Chart_Name = case_when(
             grepl("mentions", Chart_Name) ~ "Mention of cause of death",
             TRUE ~ Chart_Name)) %>% 
    dplyr::select(!c(days_in_week)) %>% 
    rename(Place_of_Death = POD_out,
           Week_End = date,
           Mention_of_Covid = covid,
           Cause_Name = name_of_cause,
           Region_Code = RGN09CD)
  
  if (remove_expected_registered == TRUE) {
    collated_output <- collated_output %>% 
      dplyr::select(!c(registered, expected))
  }
    
  filepath <- paste0(folder_path, "/weekly_chart_data.csv")
  write.csv(collated_output,
            filepath,
            row.names = FALSE)
  return(filepath)
  
}
