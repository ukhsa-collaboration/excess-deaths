# Functions to build modelling table ---------------------------------------

#' applies variables indicating the days which are in a week where Christmas day
#' falls on a Saturday, Sunday or Monday. Weeks are Saturday to Friday.
#' @param data tibble containing a date field
#' @param date_field unquoted name of the field in data which corresponds to the dates
#' 
sat_to_mon_xmas <- function(data, date_field) {
  date_range <- data %>% 
    pull({{ date_field }}) %>% 
    range()
  
  dates_in_week_where_xmas_on_sat_to_mon <- christmas_week(year(date_range[1]):year(date_range[2]),
                                                           c(1, 2, 7))
  
  data <- data %>% 
    mutate(
      sat_to_mon_xmas = 
        {{ date_field }} %in% dates_in_week_where_xmas_on_sat_to_mon,
      week_after_sat_to_mon_xmas = 
        {{ date_field }} %in% (dates_in_week_where_xmas_on_sat_to_mon + 7),
      week2_after_sat_to_mon_xmas = 
        {{ date_field }} %in% (dates_in_week_where_xmas_on_sat_to_mon + 14)
    )
  return(data)
  
}

#' creates a weekly table of bank holiday variables for modelling or predicting
#' @param from_date date; first date in time period
#' @param end_date date; last date in time period
#' @param holidays vector of dates that represent national holidays
weekly_holiday_variables <- function(from_date, to_date, holidays) {
  # Parameters to make
  # easter_pre
  # easter_post_1
  # easter_post_2
  # wk_nearest_BH
  # wk_next_nearest_BH
  # wk_fri_xmas - this is included in easter_pre
  # wk_post_fri_xmas - this is included in easter_post_1
  # wk2_post_fri_xmas - this is included in easter_post_2
  # wk_sat_to_mon_xmas
  # wk_post_sat_to_mon_xmas
  # wk2_post_sat_to_mon_xmas
  
  
  # dates need to start on a Sat
  from_date <- round_up_to_saturday(from_date)
  
  # dates need to end on a Fri
  to_date <- round_to_friday(to_date,
                             direction = "down")
  
  # create all dates by day in time period
  all_dates <- seq(
    from = from_date,
    to = to_date,
    by = "days"
  )
  
  # create vector of easter fridays
  easter_fridays <- calc_easter_fridays(holidays,
                                        include_christmas_friday = TRUE)
  
  # generate daily holiday variables
  holiday_variables <- tibble(
    date = all_dates
  ) %>% 
    # add easter binary variables:
    # days before easter friday
    # days after easter Monday
    # days the week following easter Monday
    add_easter_binary_variables(
      date_field = date,
      easter_fridays = easter_fridays
    ) %>% 
    mutate(day = wday(date)) %>%
    # create bank hol variables that aren't easter or Christmas
    add_bh_binary_variables(
      date_field = date,
      day_field = day,
      holidays = holidays
    ) %>% 
    sat_to_mon_xmas(
      date_field = date
    ) %>% 
    # round up the date field to the following Friday
    mutate(
      date = round_to_friday(
        dt = date,
        direction = "up"
      )
    ) %>% 
    group_by(date) %>% 
    summarise(
      easter_pre = max(ThurpreE, na.rm = TRUE),
      easter_post_1 = max(TuespostE, na.rm = TRUE),
      easter_post_2 = max(MonpostE1, na.rm = TRUE),
      wk_nearest_BH = max(BH_nearest_WD, na.rm = TRUE),
      wk_next_nearest_BH = max(BH_next_nearest_WD, na.rm = TRUE),
      wk_sat_to_mon_xmas = max(sat_to_mon_xmas, na.rm = TRUE),
      wk_post_sat_to_mon_xmas = max(week_after_sat_to_mon_xmas, na.rm = TRUE),
      wk2_post_sat_to_mon_xmas = max(week2_after_sat_to_mon_xmas, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  
  return(holiday_variables)
}

#' creates a weekly table mean number of days in that week since 31st Dec 2016
#' @param from_date date; first date in time period
#' @param end_date date; last date in time period
weekly_trend_variable <- function(from_date, to_date) {
  
  # dates need to start on a Sat
  from_date <- round_up_to_saturday(from_date)
  
  # dates need to end on a Fri
  to_date <- round_to_friday(to_date,
                             direction = "down")
  
  # create all dates by day in time period
  all_dates <- seq(
    from = from_date,
    to = to_date,
    by = "days"
  )
  
  trend <- tibble(
    date = all_dates,
    years_from_20161231 = as.numeric(date - as.Date("2016-12-31")) / 365.25) %>% 
    mutate(
      date = round_to_friday(
        dt = date,
        direction = "up"
      )
    ) %>% 
    group_by(date) %>% 
    summarise(
      years_from_20161231 = mean(years_from_20161231)
    )
  return(trend)
}

#' creates table of month variables (ie, seasonal) aggregated to week
#' @param from_date date; first date in time period
#' @param end_date date; last date in time period
weekly_seasonal_variables <- function(from_date, to_date) {
  # dates need to start on a Sat
  from_date <- round_up_to_saturday(from_date)
  
  # dates need to end on a Fri
  to_date <- round_to_friday(to_date,
                             direction = "down")
  
  # create all dates by day in time period
  all_dates <- seq(
    from = from_date,
    to = to_date,
    by = "days"
  ) 
  
  seasonal <- tibble(
    date = all_dates
  ) %>% 
    add_day_weighting(
      date_field = date
    ) %>% 
    pivot_wider(
      names_from = month,
      names_prefix = "month",
      values_from = month_val,
      values_fill = list(month_val = 0)
    ) %>% 
    mutate(
      date = round_to_friday(
        dt = date,
        direction = "up"
      )
    ) %>% 
    group_by(date) %>% 
    summarise(
      across(starts_with("month"),
             mean),
      .groups = "drop"
    )
  
  expected_months <- paste0("month", 1:12)
  missing_months <- setdiff(expected_months,
                            names(seasonal))
  seasonal[missing_months] <- 0   
  
  return(seasonal)
  
}

#' @description adds a weighting score between 0 and 1 to indicate how much of a
#'   weight the month that day falls into should influence the model
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


#' @description build a data frame for prediction dates with same structure as
#'   modelled data that model can be used to predict deaths on
#' @param area_codes character vector of area codes to generate data frame for
#' @inheritParams create_baseline
#' @inheritParams generate_visualisations
#' @param utla_lkp a tibble lookup table for utla to region (see utla_lookup())
build_recent_dates <- function(area_codes, from_date, to_date, holidays, 
                               denominators, utla_lkp, eth_dep, 
                               age_filter = NULL, age_group_type = "original",
                               bespoke_age_groups = NULL) {
  
  
  # dates need to start on a Sat
  from_date <- round_up_to_saturday(from_date)
  weekly_from_date <- round_to_friday(from_date,
                                      direction = "up")
  # dates need to end on a Fri
  to_date <- round_to_friday(to_date,
                             direction = "down")
  
  daily_dates <- data.frame(date = seq(from = from_date, 
                                       to = to_date,
                                       by = 'days'))
  
  weekly_dates <- data.frame(date = seq(from = weekly_from_date, 
                                        to = to_date,
                                        by = 'weeks'))
  
  # create subpopulation variables
  
  utlas <- utla_lkp %>%
    filter(!(UTLAApr19CD %in% c("E06000053", "E09000001"))) %>%
    pull(UTLAApr19CD)
  
  rgns <- utla_lkp %>%
    pull(RGN09CD) %>%
    unique()
  
  Sex <- factor(c(0, 1))
  
  Age_Group <- age_group_lkp(age_filter = age_filter, 
                             type = age_group_type,
                             bespoke_age_groups = bespoke_age_groups) %>%
    pull(Age_Group) %>%
    unique() %>%
    factor()
  
  if (eth_dep == TRUE) {
    weekly_dates <- weekly_dates %>%
      merge(y = data.frame(RGN09CD = area_codes, stringsAsFactors = FALSE)) %>%
      merge(y = data.frame(Ethnic_Group = factor(c("Asian", "Black", "Mixed", "Other", "White")))) %>%
      merge(y = data.frame(Deprivation_Quintile = factor(1:5)))
    
  } else {
    weekly_dates <- weekly_dates %>%
      merge(y = data.frame(UTLAApr19CD = utlas, stringsAsFactors = FALSE))
  }
  
  weekly_dates <- weekly_dates  %>%
    merge(y = data.frame(Sex = Sex)) %>%
    merge(y = data.frame(Age_Group = Age_Group))
  
  # create denominator variable
  # aggregate denominator data to weekly
  denominators <- weekly_denominators(
    denominators = denominators,
    from_date = from_date,
    to_date = to_date
  )
  
  if (eth_dep == TRUE) {
    denominators <- denominators %>% 
      rename(RGN09CD = OfficialCode)
  } else if (eth_dep == FALSE) {
    denominators <- denominators %>% 
      rename(UTLAApr19CD = OfficialCode)
  }
  
  # create date-dependent variables
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
  
  
  # join variables together
  recent_dates <- weekly_dates %>% 
    left_join(denominators, by = intersect(names(.), 
                                           names(denominators))) %>% 
    left_join(date_dependent_variables,
              by = "date") %>% 
    mutate(deaths_total = NA)
  
  return(recent_dates)
}

#' aggregates denominator table containing monthly denominator data to a daily
#' version, then removes the weekends and bank holidays before aggregating it to
#' a monthly version
#' @param denominators tibble; the tibble must contain fields for month,
#'   denominator, and any other population subgroup category desired (eg, age,
#'   sex, ethnic group, deprivation quintile)
#' @param from_date date; earliest date in the period of concern
#' @param to_date date; latest date in the period of concern
weekly_denominators <- function(denominators, from_date, to_date) {
  
  # dates need to start on a Sat
  from_date <- round_up_to_saturday(from_date)
  
  # dates need to end on a Fri
  to_date <- round_to_friday(to_date,
                             direction = "down")
  
  # create all dates by day in time period
  all_dates <- seq(
    from = from_date,
    to = to_date,
    by = "days"
  ) 
  
  holidays <- holiday_dates(
    from_date = from_date,
    to_date = to_date
  )
  
  weekly_denominators <- tibble(
    date = all_dates,
    month = floor_date(date, 
                       unit = "month")
  ) %>% 
    inner_join(
      denominators,
      by = "month"
    ) %>% 
    remove_we_bh_denominators(
      denominator_field = denominator,
      date_field = date,
      holidays = holidays) %>% 
    mutate(
      date = round_to_friday(
        dt = date,
        direction = "up"
      )
    ) %>% 
    group_by(across(!c(month, denominator))) %>% 
    summarise(
      denominator = sum(denominator),
      .groups = "drop"
    )
  
  return(weekly_denominators)
}

#' removes denominators from weekend and bank holiday dates. Within the
#' baseline, death period, deaths were very unlikely to be registered on
#' weekends or bank holidays therefore these days should have consistent
#' predictions with working days
#' @param data tibble of data where one field contains the denominator
#' @param denominator_field unquoted field name of the field containing the
#'   denominator
#' @param date_field unquoted field name of the field containing the date
#' @param holidays vector of dates which are bank holidays
remove_we_bh_denominators <- function(data, denominator_field, date_field, holidays) {
  data <- data %>% 
    mutate({{ denominator_field }} := case_when(
      wday({{ date_field }}) %in% c(1, 7) ~ 0,
      {{ date_field }} %in% holidays ~ 0,
      TRUE ~ as.numeric({{ denominator_field }})
    ))
  
  return(data)
}

#' @description adds binary variables for days around bank holidays that aren't
#'   easter or xmas. Bank holidays that occur on Tuesday, Wednesday or Thursday
#'   aren't picked out as it is assumed that the variability in registrations
#'   occurs within the same week that the bank holiday occurred
add_bh_binary_variables <- function(data, date_field, day_field, holidays) {
  easter_fridays <- calc_easter_fridays(holidays)
  
  non_easter_holidays <- holidays[!(holidays %in% c(easter_fridays, (easter_fridays + 3)))]
  
  friday_bhols <- non_easter_holidays[wday(non_easter_holidays) == 6 & 
                                        month(non_easter_holidays) != 12]
  monday_bhols <- non_easter_holidays[wday(non_easter_holidays) == 2 & 
                                        month(non_easter_holidays) != 12]
  
  
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

# QA functions ------------------------------------------------------------


#' This function that compares the OHID regional total, covid, expected, and
#' excess deaths against ONS's and outputs a qa csv file to be copied into the
#' monthly JIRA ticket.
#'
#' This function saves the comparison table and the ONS weekly regional deaths
#' data at: //FilePor10/DataLake$/Y124_COVID19PHAIneq_Pseudo/Excess
#' mortality/powerbi/qa/compare_regional_to_ons/
compare_regional_to_ons <- function() {
  # get ONS total deaths and ONS covid deaths
  ons_deaths_url <- paste0("https://www.ons.gov.uk/peoplepopulationandcommunity/",
                           "birthsdeathsandmarriages/deaths/datasets/",
                           "weeklyprovisionalfiguresondeathsregisteredinenglandandwales")
  
  links <- rvest::read_html(ons_deaths_url) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    .[grepl("csv|xls.$", .)] %>%
    .[1]
  
  url <- paste0("https://www.ons.gov.uk", gsub("%2f", "/", links))
  report_file <- basename(links)
  destfile <- paste0(Sys.getenv("POWERBI_FILESHARE"), 
                     "/qa/compare_regional_to_ons/", 
                     report_file)
  download.file(url, destfile = destfile, mode = "wb",
                quiet = TRUE)
  
  ons_df <- readxl::read_xlsx(paste0(destfile),
                              sheet = "12a", # selects the registrations worksheet
                              skip = 6
  ) %>%
    rename("week_number" = "Week number", "date" = "Week ending") %>%
    filter(!stringr::str_detect(week_number, "Week number")) %>%
    drop_na() %>%
    mutate(across(where(is.character), as.numeric),
           date = as.Date(date, origin = "1899-12-30"))
  
  latest_ons_week_no <- max(ons_df$week_number)
  latest_ons_week <- max(ons_df$date)
  
  ons_deaths <- ons_df %>%
    filter(week_number == latest_ons_week_no) %>%
    mutate(name = case_when(
      row_number() == 1 ~ "Total deaths ONS",
      row_number() == 2 ~ "Covid deaths ONS"
    )) %>%
    select(c("name",
             "North East",
             "North West",
             "Yorkshire and The Humber",
             "East Midlands",
             "West Midlands",
             "East of England",
             "London",
             "South East",
             "South West")) %>%
    pivot_longer(-name, names_to = "RGN09NM") %>%
    pivot_wider(names_from = name, values_from = value)
  
  # get OHID/PHDS total, covid, expected, and excess deaths
  ohid_df <- read_csv(paste0(Sys.getenv("LKIS_FILESHARE"),
                             "reg_exdeaths_pbi_sf.csv"),
                      show_col_types = FALSE) %>%
    filter(type == "region",
           date == max(date),
           RGN09NM != "England") 
  
  latest_ohid_week <- max(ohid_df$date)
  
  ohid_df <- ohid_df %>%
    select(c("RGN09NM",
             "all_dths",
             "covid_dths",
             "exptd_dths",
             "tot_excess",
             "excess_deficit")) %>%
    mutate(tot_excess = case_when(tot_excess > 0 ~ tot_excess,
                                  excess_deficit < 0 ~ excess_deficit,
                                  TRUE ~ tot_excess)) %>%
    select(c("RGN09NM",
             "all_dths",
             "covid_dths",
             "exptd_dths",
             "tot_excess")) %>%
    rename("Total deaths OHID" = all_dths,
           "Covid deaths OHID" = covid_dths,
           "Expected deaths OHID" = exptd_dths,
           "Excess deaths OHID" = tot_excess) %>%
    mutate(across(where(is.numeric), round, 0))
  
  
  # get LKIS/ONS expected deaths for 2015 to 2019
  # 2015 to 2019 average deaths csv copied from 
  # \\filewms08\K&I\LKIS\1. Analysis\1. Resources\\Covid19_ONSMortality\Weekly_LA_reports\Death average\output
  lkis_expected_df <- read_csv(paste0(Sys.getenv("POWERBI_FILESHARE"),
                                      "/qa/compare_regional_to_ons/",
                                      "wkdths_avg1519_pod.csv"),
                               show_col_types = FALSE) %>%
    filter(Area_code %in% c("E12000001",
                            "E12000002",
                            "E12000003",
                            "E12000004",
                            "E12000005",
                            "E12000006",
                            "E12000007",
                            "E12000008",
                            "E12000009")) %>%
    group_by(Area_name, WkNo) %>%
    summarise(reg = sum(reg) / 5,
              .groups = "drop_last")
  
  lkis_expected_current_wk <- lkis_expected_df %>%
    filter(WkNo == latest_ons_week_no) %>%
    select(Area_name, reg) %>%
    rename("Expected deaths ONS" = reg,
           "RGN09NM" = "Area_name") %>%
    mutate(across(where(is.numeric), round, 0))
  
  # combine three tables and calculate ONS excess deaths
  qa_table <- merge(ons_deaths, ohid_df, by = "RGN09NM") %>%
    merge(lkis_expected_current_wk, by = "RGN09NM") %>%
    mutate("Excess deaths ONS" = `Total deaths ONS` - `Expected deaths ONS`,
           "Total deaths diff" = `Total deaths OHID` - `Total deaths ONS`,
           "Covid deaths diff" = `Covid deaths OHID` - `Covid deaths ONS`,
           "Expected deaths diff" = `Expected deaths OHID` - `Expected deaths ONS`,
           "Excess deaths diff" = `Excess deaths OHID` - `Excess deaths ONS`) %>%
    select("RGN09NM",
           "Total deaths OHID",
           "Total deaths ONS",
           "Total deaths diff",
           "Covid deaths OHID",
           "Covid deaths ONS",
           "Covid deaths diff",
           "Expected deaths OHID",
           "Expected deaths ONS",
           "Expected deaths diff",
           "Excess deaths OHID",
           "Excess deaths ONS",
           "Excess deaths diff") %>%
    pivot_longer(-RGN09NM) %>%
    pivot_wider(names_from = RGN09NM, values_from = value) %>%
    mutate(`England (aggregated)` = case_when(name %in% c("Total deaths OHID",
                                                          "Total deaths ONS",
                                                          "Covid deaths OHID",
                                                          "Covid deaths ONS",
                                                          "Expected deaths OHID",
                                                          "Expected deaths ONS",
                                                          "Excess deaths OHID",
                                                          "Excess deaths ONS") ~ as.character(rowSums(.[2:10])),
                                              TRUE ~ " ")) %>%
    mutate(name = case_when(name == "Expected deaths ONS" ~ "Expected deaths ONS (2015-19)",
                            name == "Excess deaths ONS" ~ "Excess deaths ONS (2015-19)",
                            TRUE ~ name)) %>%
    rename(" " = "name")
  
  
  if (latest_ons_week == latest_ohid_week) {
    print(paste0("The latest week ending dates for both ONS and OHID data match - ", 
                 latest_ons_week))
  } else {
    print(paste0("The latest week ending dates for ONS, ",
                 latest_ons_week,
                 ", and OHID data ",
                 latest_ohid_week,
                 ", DO NOT MATCH"))
  }
  
  write.csv(qa_table, paste0(Sys.getenv("POWERBI_FILESHARE"), 
                             "/qa/compare_regional_to_ons/compare_regional_to_ons_",
                             latest_ohid_week,
                             ".csv"),
            row.names = FALSE)
  
  print(paste0("Comparison table stored here:",
               paste0(Sys.getenv("POWERBI_FILESHARE"), 
                      "/qa/compare_regional_to_ons/compare_regional_to_ons_",
                      latest_ohid_week,
                      ".csv")))
  return(qa_table)
}

compare_this_and_last_weeks_file <- function(path, geography = c("england", "region")) {
  
  # ensure test file compared with live file
  last_versions_path <- gsub("_test",
                             "",
                             path)
  
  # check geography argument
  geography <- match.arg(geography)
  
  if (geography == "england") {
    old_path <- str_replace(last_versions_path, 
                            date_as_string(last_versions_path, week_type = "this week"),
                            date_as_string(last_versions_path, week_type = "last week"))
    
    common_cols <- c("type", "Plot_Label", "Sex")
  } else if (geography == "region") {
    old_path <- str_replace(last_versions_path, 
                            date_as_string(last_versions_path, week_type = "this week"),
                            date_as_string(last_versions_path, week_type = "first friday last month"))
    
    common_cols <- c("type", "Plot_Label", "RGN09CD", "RGN09NM", "Sex")
  }
  
  
  
  # calculate the totals for each subgroup ----------------------------------
  
  compare_cols <- c(registered = "all_dths", 
                    expected = "exptd_dths", 
                    covid_mentions = "covid_dths", 
                    underlying_specific_cause = "ucod_disease", 
                    underlying_covid = "ucod_covid")
  
  unnamed_compare_cols <- unname(compare_cols)
  
  old_file_totals <- read.csv(old_path)
  
  dates_and_types <- old_file_totals %>% 
    distinct(type, date)
  
  old_file_totals <- old_file_totals %>% 
    group_by(across(all_of(common_cols))) %>% 
    summarise(across(all_of(unnamed_compare_cols),
                     sum),
              .groups = "drop") %>% 
    rename_with(~ paste0(.x, ".old"), all_of(unnamed_compare_cols))
  
  
  new_file_totals <- read.csv(path) %>% 
    inner_join(dates_and_types, by = c("type", "date")) %>% 
    group_by(across(all_of(common_cols))) %>% 
    summarise(across(all_of(unnamed_compare_cols),
                     sum),
              .groups = "drop") %>% 
    rename_with(~ paste0(.x, ".new"), all_of(unnamed_compare_cols))
  
  data_compared <- new_file_totals %>% 
    left_join(old_file_totals, by = common_cols) %>% 
    tidyr::pivot_longer(cols = !all_of(common_cols),
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
    dplyr::select(all_of(c(common_cols, "count_type", "new_minus_old")))
  
  return(data_compared)
}

qa_power_bi_file <- function(filepath, geography = "england") {
  df <- read.csv(filepath)
  
  compare_cols <- c("all_dths", "exptd_dths", "covid_dths")
  
  if (geography == "england") {
    all_persons_grouping <- NULL
    totals_grouping <- "type"
    rgn_code <- "E92000001"
    join_fields <- "death_type"
  } else if (geography == "region") {
    all_persons_grouping <- c("RGN09CD", "RGN09NM")
    totals_grouping <- c("RGN09CD", "RGN09NM", "type")
    rgn_code <- paste0("E1200000", 1:9)
    join_fields <- c("RGN09CD", "RGN09NM", "death_type")
  }
  
  # check non-ethnic group totals -------------------------------------------
  
  all_person_totals <- df %>% 
    filter(type == "England") %>% 
    group_by(across(all_of(all_persons_grouping))) %>% 
    summarise(across(all_of(compare_cols),
                     sum),
              .groups = "drop")
  
  total_checks <- df %>% 
    filter(Sex == "Persons",
           RGN09CD %in% rgn_code,
           !(type %in% c("England", "all cause")),
           across(
             all_of(
               c("Ethnic_Group", "Age_Group", "POD_out", "Deprivation_Quintile")
             ),
             function(x) x %in% c(" All", "All")
           )) %>% 
    group_by(across(all_of(totals_grouping))) %>% 
    summarise(across(all_of(compare_cols),
                     sum),
              .groups = "drop") %>% 
    mutate(description = "Totals value for subpopulation")
  
  total_checks_ethnicity <- total_checks %>% 
    filter(type == "Ethnicity")
  
  total_checks_deprivation <- total_checks %>% 
    filter(type == "deprivation")
  
  total_checks <- total_checks %>% 
    filter(!(type %in% c("Ethnicity", "deprivation")))
  
  subgroup_age_checks <- df %>% 
    filter(type == "Age_Group",
           Age_Group != " All") %>% 
    mutate(Sex = if_else(Sex == "Persons", "By person", "By gender")) %>% 
    group_by(across(all_of(c(totals_grouping, "Sex")))) %>% 
    summarise(across(all_of(compare_cols),
                     sum),
              .groups = "drop") %>% 
    mutate(description = "Subgroups of subpopulation aggregated")
  
  subgroup_pod_checks <- df %>% 
    filter(type == "pod",
           POD_out != "All") %>% 
    group_by(across(all_of(totals_grouping))) %>% 
    summarise(across(all_of(compare_cols),
                     sum),
              .groups = "drop") %>% 
    mutate(description = "Subgroups of subpopulation aggregated")
  
  if (geography == "england") {
    subgroup_region_checks <- df %>% 
      filter(type == "region",
             RGN09CD != "E92000001") %>% 
      group_by(across(all_of(totals_grouping))) %>% 
      summarise(across(all_of(compare_cols),
                       sum),
                .groups = "drop") %>% 
      mutate(description = "Subgroups of subpopulation aggregated")
    
  }
  
  if (geography == "england") {
    subgroup_utla_checks <- df %>% 
      filter(type == "utla") %>% 
      group_by(across(all_of(totals_grouping))) %>% 
      summarise(across(all_of(compare_cols),
                       sum),
                .groups = "drop") %>% 
      mutate(description = "Subgroups of subpopulation aggregated")
  } else if (geography == "region") {
    utla_lkp <- utla_lookup()
    subgroup_utla_checks <- df %>% 
      filter(type == "utla") %>% 
      rename(UTLAApr19CD = RGN09CD,
             UTLAApr19NM = RGN09NM) %>% 
      left_join(utla_lkp, by = c("UTLAApr19CD", "UTLAApr19NM")) %>% 
      group_by(across(all_of(totals_grouping))) %>% 
      summarise(across(all_of(compare_cols),
                       sum),
                .groups = "drop") %>% 
      mutate(description = "Subgroups of subpopulation aggregated")
    
  }
  
  non_eth_dep_combined <- bind_rows(
    total_checks,
    subgroup_age_checks,
    subgroup_pod_checks,
    subgroup_utla_checks
  )
  
  if (geography == "england") {
    non_eth_dep_combined <- bind_rows(
      non_eth_dep_combined,
      subgroup_region_checks
    )
  }
  
  pivot_longer_compare_wider <- function(data1, data2, pivot_cols, join_fields) {
    
    data2 <- data2 %>% 
      pivot_longer(cols = all_of(pivot_cols),
                   names_to = "death_type",
                   values_to = "count2")
    
    combined <- data1 %>% 
      pivot_longer(cols = all_of(pivot_cols),
                   names_to = "death_type",
                   values_to = "count1") %>% 
      left_join(data2, by = join_fields) %>% 
      mutate(difference = count1 - count2) %>% 
      dplyr::select(!c(count1, count2)) %>% 
      pivot_wider(names_from = "death_type",
                  values_from = "difference")
    
    return(combined)
    
  }
  
  non_eth_dep_combined <- pivot_longer_compare_wider(
    data1 = non_eth_dep_combined,
    data2 = all_person_totals,
    pivot_cols = compare_cols,
    join_fields = join_fields
  )
  
  # check ethnic group totals -----------------------------------------------
  
  all_person_ethnicity_totals <- df %>% 
    mutate(date = as.Date(date)) %>% 
    filter(type == "England", 
           date != max(date)) %>% 
    group_by(across(all_of(all_persons_grouping))) %>% 
    summarise(across(all_of(compare_cols),
                     sum),
              .groups = "drop")
  
  subgroup_ethnicity_checks <- df %>% 
    filter(type == "Ethnicity",
           Ethnic_Group != "All") %>% 
    mutate(Sex = if_else(Sex == "Persons", "By person", "By gender")) %>% 
    group_by(across(all_of(c(totals_grouping, "Sex")))) %>% 
    summarise(across(all_of(compare_cols),
                     sum),
              .groups = "drop") %>% 
    mutate(description = "Subgroups of subpopulation aggregated")
  
  ethnicity_combined <- bind_rows(
    total_checks_ethnicity,
    subgroup_ethnicity_checks
    
  )
  
  ethnicity_combined <- pivot_longer_compare_wider(
    data1 = ethnicity_combined,
    data2 = all_person_ethnicity_totals,
    pivot_cols = compare_cols,
    join_fields = join_fields
  )
  
  # check deprivation totals ------------------------------------------------
  
  all_person_deprivation_totals <- df %>% 
    mutate(date = as.Date(date)) %>% 
    filter(type == "England") %>% 
    group_by(across(all_of(all_persons_grouping))) %>% 
    summarise(across(all_of(compare_cols),
                     sum),
              .groups = "drop")
  
  subgroup_deprivation_checks <- df %>% 
    filter(type == "deprivation",
           Deprivation_Quintile != "All") %>% 
    mutate(Sex = if_else(Sex == "Persons", "By person", "By gender")) %>% 
    group_by(across(all_of(c(totals_grouping, "Sex")))) %>% 
    summarise(across(all_of(compare_cols),
                     sum),
              .groups = "drop") %>% 
    mutate(description = "Subgroups of subpopulation aggregated")
  
  deprivation_combined <- bind_rows(
    total_checks_deprivation,
    subgroup_deprivation_checks
  )
  
  deprivation_combined <- pivot_longer_compare_wider(
    data1 = deprivation_combined,
    data2 = all_person_deprivation_totals,
    pivot_cols = compare_cols,
    join_fields = join_fields
  )
  
  all_comparisons <- bind_rows(
    non_eth_dep_combined,
    ethnicity_combined,
    deprivation_combined
  ) %>% 
    mutate(across(all_of(compare_cols),
                  ~ round_correct(.x, n = 4)))
  return(all_comparisons)
}



# power bi functions ------------------------------------------------------

#' @description creates the lookup table required for dates in the regional power bi report
#' @return table of 3 columns, one which is the selectable date for the user,
#'   one which is all the different Friday dates between 27 March 2020 and the
#'   selectable date, and one which is an ordering term so the dates are sorted
#'   in reverse chronological order when visible in the power bi tool
generate_regional_date_csv_lookup <- function() {
  first_reg_report_date <- as.Date("2020-07-31")
  second_reg_report_date <- "2020-11-06"
  
  # create a list of dates from second report to final report date
  report_dates <- seq(from = as.Date(second_reg_report_date), 
                      to = as.Date(final_date), 
                      by = "1 day")
  
  # filter date list for only first Fridays of the month
  report_dates <- report_dates[wday(report_dates, label = TRUE) == "Fri" & 
                                 day(report_dates) <= 7]
  
  # if the first Friday is on 1st of Month, change to 8th
  report_dates <- ifelse(day(report_dates) == 1, 
                         as.Date(report_dates + 7), 
                         report_dates) %>%
    as.Date(origin = "1970-01-01")
  
  # add 31 July 2020 report
  report_dates <- c(first_reg_report_date, report_dates)
  
  # add column the weekly Fridays for each end_date
  full_dates <- purrr::map_dfr(report_dates, add_dates_from_march_2020) %>% 
    mutate(order = factor(end_date),
           order = factor(order,
                          levels = rev(levels(order))), # make most recent date the first level
           order = as.numeric(order), # use the numeric value of the factor
           end_date = format(end_date, format="%d %B %Y")) # make date format readable for tool user
  
  
  write.csv(full_dates,
            paste0(Sys.getenv("LKIS_FILESHARE"),
                   "/reg_pbix_dates.csv"), 
            row.names = FALSE)
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


#' @description using a date input, the function creates a table where the date
#'   is an output alongside all of the weeks between 27 March 2020 and that
#'   date. This is required for the regional power bi document toallow for the
#'   data to be displayed for different overlapping time periods
add_dates_from_march_2020 <- function(date) {
  date_table <- tibble(
    end_date = date,
    week_ends = seq(from = as.Date("2020-03-27"),
                    to = as.Date(date),
                    by = "week")
  )
  return(date_table)
}

#' @description export data ready for using in PowerBI
export_powerbi_data <- function(data, grouping_fields, model_filename, from_date, end_date,
                                directory = Sys.getenv("POWERBI_FILESHARE"), age_filter = NULL,
                                breakdown_by_ucod = FALSE, report_type = "live") {
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
  
  suffix <- ""
  if (report_type == "test") suffix <- "_test"
  
  filename  <- paste0(filename,
                      "_ons_aligned_weekly", 
                      suffix,
                      ".csv")
  write.csv(data, 
            filename,
            row.names = FALSE)
}

# lookup functions --------------------------------------------------------


#' @description retrieve a UTLA code, UTLA Name, Region code and Region Name
#'   lookup from the Data Lake
utla_lookup <- function() {
  con <- dbConnect(odbc(), 
                   Driver = "SQL Server", 
                   Server = Sys.getenv("DATA_LAKE_SERVER"), 
                   Database = Sys.getenv("LOOKUPS_DATABASE"), 
                   Trusted_Connection = "True",
                   timeout = 120)
  
  lkup <- tbl(con, 
              in_schema(
                sql(Sys.getenv("LOOKUPS_DATABASE")), 
                sql(Sys.getenv("UTLA19_LKP_TABLE")))
  )
  
  lkup_table <- lkup %>%
    dplyr::select(UTLAApr19CD = UTLA19CD, UTLAApr19NM = UTLA19NM, RGN09CD, RGN09NM) %>%
    collect() %>%
    unique()
  dbDisconnect(con)
  return(lkup_table)
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

#' @description a cause code to disease lookup table
#' @param cause_name character vector to filter output with
#' @param table_output logical; FALSE returns vector of ICD codes, TRUE returns
#'   a table with cause_name and ICD codes
cause_code_lookup <- function(cause_name = NULL, table_output = FALSE,
                              include_epi_hf = FALSE, detail_cancers = FALSE) {
  ihd_codes <- paste0("I", 20:25)
  cereberovascular_codes <- paste0("I", 60:69)
  other_circulatory_codes <- paste0("I", formatC(0:99, width = 2, flag = "0"))
  other_circulatory_codes <- other_circulatory_codes[!(other_circulatory_codes %in% c(ihd_codes,
                                                                                      cereberovascular_codes))]
  cancer_codes <- tibble(
    ICD_codes = paste0("C", formatC(0:97, width = 2, flag = "0")),
    name_of_cause = "cancer"
  )
  
  if (detail_cancers == TRUE) {
    cancer_codes <- cancer_codes %>% 
      mutate(
        name_of_cause = case_when(
          ICD_codes %in% c("C33", "C34") ~ "tracheal_bronchus_lung_cancer",
          ICD_codes %in% paste0("C", 18:21) ~ "colon_sigmoid_rectum_anus_cancer",
          ICD_codes %in% paste0("C", 81:96) ~ "lymphoid_haemotopoietic_related_cancer",
          # ICD_codes %in% c("C61") ~ "prostate_cancer",
          ICD_codes %in% c("C50") ~ "breast_cancer",
          # ICD_codes %in% c("C25") ~ "pancreas_cancer",
          TRUE ~ "other cancers"
        )
      )
  }
  
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
  
  cause_codes <- cause_codes %>% 
    anti_join(cancer_codes, by = "ICD_codes") %>% 
    bind_rows(cancer_codes)
  
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

all_models <- function(model_name = NULL) {
  
  model_refs <- tibble::tribble(
    ~reference,                                                                          ~prediction_file,
    # "ARI mentions",                                                                "20210113_ari_mentions",
    # "Dementia mentions",                                                      "20210113_dementia_mentions",
    # "Other causes (excl. COVID-19) mentions",                                      "20210113_all_other_causes", 
    # "Epilepsy mentions",                                                      "20210302_epilepsy_mentions", 
    "Parkinson's disease mentions",                                 "20211201_parkinsons_disease_mentions", 
    "Diabetes mentions",                                                      "20211201_diabetes_mentions",
    "Cirrhosis and other liver diseases mentions",  "20211201_cirrhosis_and_other_liver_diseases_mentions", 
    "Diseases of the urinary system mentions",          "20211201_diseases_of_the_urinary_system_mentions", 
    "Dementia and Alzheimer's mentions",                       "20211201_dementia_and_alzheimers_mentions", 
    "Other respiratory diseases mentions",                  "20211201_other_respiratory_diseases_mentions", 
    "Chronic lower respiratory diseases mentions",  "20211201_chronic_lower_respiratory_diseases_mentions", 
    "Acute respiratory infections mentions",              "20211201_acute_respiratory_infections_mentions", 
    "Cancer mentions",                                                          "20211201_cancer_mentions",
    "Heart failure mentions",                                            "20211202_heart_failure_mentions",
    "Other circulatory diseases mentions",                  "20211201_other_circulatory_diseases_mentions",
    "Cerebrovascular diseases mentions",                      "20211201_cerebrovascular_diseases_mentions", 
    "Ischaemic heart diseases mentions",                      "20211201_ischaemic_heart_diseases_mentions",                  
    "Other places POD",                                                            "20211201_other_places",
    "Hospice POD",                                                                      "20211201_hospice",
    "Hospital (acute or community, not psychiatric) POD",                              "20211201_hospital",
    "Care home (nursing or residential) POD",                                         "20211201_care_home",
    "Home POD",                                                                            "20211201_home",
    # "England",                                                                          "20210212_eth_dep",
    # "Region",                                                                           "20210212_eth_dep",
    # "Age-Sex",                                                                          "20210212_eth_dep",
    # "Deprivation",                                                                      "20210212_eth_dep",
    # "Ethnicity-Sex",                                                                    "20210212_eth_dep",
    "Ethnicity-deprivation",                                                            "20220118_eth_dep",
    "UTLA",                                                                                "20211201_utla")
  
  
  
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


#' @description look up table for age in years to age group
#' @param age_group_type string; "original", "nomis", "bespoke" are accepted
#'   inputs. If "bespoke" is used, then the bespoke_age_groups parameter needs
#'   to be provided
#' @param bespoke_age_groups tibble; two fields - Age and Age_Group. Age contains
#'   integers and Age_Group is a character class displaying the age group that
#'   that age is assigned to
#'
#'   
age_group_lkp <- function(age_filter = NULL, type = "original", bespoke_age_groups) {
  
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
  } else if (type == "bespoke") {
    if (missing(bespoke_age_groups)) stop("bespoke_age_groups must be a tibble with fields Age and Age_Group")
    if ((any(!names(bespoke_age_groups) %in% c("Age", "Age_Group"))))
      stop("bespoke_age_groups must be a tibble with fields Age and Age_Group")
    agegroup_lkp <- bespoke_age_groups
  }
  
  return(agegroup_lkp)
}

#' @description ethnic group lookup table
ethnic_groups_lookup <- function() {
  ethnic_groups <- tibble::tribble(
    ~Ethnic_Group,                    ~Ethnic_group_data_lake,                       ~Ethnicity_Broad,                                                                         ~Ethnicity_Nomis,
    "Asian",                          "Asian / Asian British",                       "2-Asian or Asian British including Chinese",                                             "Asian/Asian British: Total",
    "Black",                          "Black / African / Caribbean / Black British", "3-Black or Black British",                                                               "Black/African/Caribbean/Black British: Total",
    "Mixed",                          "Mixed / Multiple ethnic groups",              "4-Mixed",                                                                                "Mixed/multiple ethnic group: Total",
    "Not_Stated/Not_Known/Not_Given", "No ethnicity information",                    "6-Not_Stated/Not_Known/Not_Given",                                                       NA,
    "Other",                          "Any other ethnic group",                      "5-Other Ethnic Groups",                                                                  "Other ethnic group: Total",
    "Unknown",                        "No ethnicity information",                    "7-NULL-SEX in mortality does match SEX in HES or Ethnicity not in HES for search FYEAR", NA,
    "White",                          "White",                                       "1-White",                                                                                "White: Total",
    "Not_Stated/Not_Known/Not_Given", "99 Not known",                                "6-Not_Stated/Not_Known/Not_Given",                                                       NA,
    "Not_Stated/Not_Known/Not_Given", "Not known",                                   "6-Not_Stated/Not_Known/Not_Given",                                                       NA,
    "Not_Stated/Not_Known/Not_Given", "Z Not stated",                                "6-Not_Stated/Not_Known/Not_Given",                                                       NA
  )
  
  return(ethnic_groups)
}

#' returns a vector of holidays in the years that the dates are specified for
#' @param from_date date; earliest date in the period of concern
#' @param to_date date; latest date in the period of concern
holiday_dates <- function(from_date, to_date) {
  # get vector of bank holidays
  holidays <- timeDate::holidayLONDON(year(from_date):year(to_date)) 
  holidays <- as.Date(holidays)
  holidays <- replace(holidays, holidays == as.Date("2020-05-04"), as.Date("2020-05-08"))
  holidays <- replace(holidays, holidays == as.Date("2022-05-30"), as.Date("2022-06-02"))
  holidays <- c(holidays, as.Date("2022-06-03"))
  
  return(holidays)
}

#' @description function that creates a vector of dates who sit in the same week
#'   as Christmas, where Christmas day is on a specified day of the week (ie, if
#'   interested in a Friday Christmas only and the years 2015 to 2020 were
#'   provided, only 19th to 25th December 2015 would be returned)
#' @param year integer; year of interest
#' @param day_of_interest integer between 1 and 7 for day of the week (1 is
#'   Sunday)
christmas_week <- function(year, day_of_interest) {
  xmas <- as.Date(paste(year, "12", "25",
                        sep = "-"))
  keep_xmas <- xmas[wday(xmas) %in% day_of_interest]
  
  friday_dates <- keep_xmas + (7 - wday(keep_xmas, week_start = 6))
  
  dates <- c(friday_dates,
             friday_dates - rep(1:6, each = length(friday_dates)))
  dates <- sort(dates)
  return(dates)
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

first_friday <- function(date) {
  if (wday(date) <= 6) {
    add_days <- 6 - wday(date)
  } else {
    add_days <- 6
  }
  date <- date + add_days
  return(date)
}

#' @description generate date to use for final date in report
final_report_date <- function() {
  final_date <- Sys.Date() - lubridate::wday(Sys.Date() + 1) - 7
  return(final_date)
}

#' collates all the dispersion parameters from the models into a tibble for
#' later use. The source data are test files stored in the
#' DISPERSION_PARAMETERS_FILESHARE, which is written to as part of the modelling
#' process
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

# transform functions -----------------------------------------------------


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



#' @description helper function to convert rds object for model filename to txt
#'   file to store the dispersion parameter
#' @inheritParams get_predictions
filename_model_to_dispersion <- function(model_filename) {
  disp_par_path <- Sys.getenv("DISPERSION_PARAMETERS_FILESHARE")
  disp_par_filename <- gsub("rds$", "txt", model_filename)
  disp_par_filename <- gsub("model_outputs", disp_par_path, disp_par_filename)
  return(disp_par_filename) 
}

capitalise_first_letter <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

date_as_string <- function(path, 
                           week_type = c("this week", "last week", "first friday last month"),
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
  } else if (week_type == "first friday last month") {
    start_of_last_month <- as.Date(
      paste(year(current_date),
            month(current_date) - 1,
            "01",
            sep = "-")
    )
    date_as_string <- first_friday(start_of_last_month) %>% 
      as.character() %>% 
      stringr::str_replace_all("-", "")
  }
  return(date_as_string)
}

#' converts a date to the following Saturday
#' @param dt date
round_up_to_saturday <- function(dt) {
  dt <- dt + (7 - wday(dt))
  return(dt)
}

#' converts a date to the previous Friday
#' @param dt date
#' @param direction string; "up" or "down" - which direction to round the date
round_to_friday <- function(dt, direction) {
  direction <- match.arg(direction,
                         c("down", "up"))
  
  days_of_week <- wday(dt)
  saturdays <- days_of_week == 7
  
  if (direction == "up") {
    not_saturdays <- days_of_week != 7
    
    dt[saturdays] <- dt[saturdays] + 6
    dt[not_saturdays] <- dt[not_saturdays] + (6 - days_of_week[not_saturdays])
  } else if (direction == "down") {
    sundays <- days_of_week == 1
    mon_to_thurs <- between(days_of_week, 2, 5)
    
    dt[saturdays] <- dt[saturdays] - 1
    dt[sundays] <- dt[sundays] - 2
    dt[mon_to_thurs] <- dt[mon_to_thurs] - (days_of_week[mon_to_thurs] + 1)
    
  }
  return(dt)
}


# removes columns in a data frame that are only NA or "All"
remove_columns_with_all_or_nas_only <- function(data, geography = "england") {
  if (geography == "england") {
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
  } else if (geography == "region") {
    data_process <- data %>% 
      dplyr::select(`Chart Group` = type,
                    `Population Group` = Chart_Name,
                    `Population Subgroup` = Plot_Label,
                    `Period Start` = Period_Start,
                    `Period End` = Period_End,
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
                    `Deaths with Specific Disease as the Underlying Cause` = ucod_disease,
                    `Ratio of registered deaths to expected deaths` = ratio)
  }
  
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


convert_age_filter <- function(age_filter) {
  lower_age <- age_filter[1]
  if (is.na(lower_age)) lower_age <- 0
  
  upper_age <- age_filter[2]
  if (is.na(upper_age)) upper_age <- 200
  
  return(list(lower_age = lower_age,
              upper_age = upper_age))
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

#' @description Manipulate deaths data so weekdays (and not bank hols) are
#'   included, along with sex only containing 0 and 1, and all data from bank
#'   hols and weekends are assigned to the previous working day. Also field
#'   added for years from end of Dec
preprocess_deaths <- function(data, date_field, death_field, sex_field, age_group_field, holidays) {
  
  data <- data %>%
    filter(Sex != 3) %>%
    mutate(day = wday({{ date_field }})) %>%
    sex_change_0_1({{ sex_field }}) %>%
    mutate({{ age_group_field }} := factor({{ age_group_field }})) %>% 
    rename(date = {{ date_field }})
  
  return(data)
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
      rename(date = Reg_Date,
             !! new_deaths_field_name := {{ deaths_field }}) %>%
      mutate(Age_Group = factor(Age_Group))
  }
  
  return(data)
}


# Modelling functions -----------------------------------------------------

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


# Check functions ---------------------------------------------------------

# return length of icd codes. If lengths are different error out.
icd_codes_length <- function(codes){
  
  nchars <- unique(nchar(codes))
  
  if (length(nchars) > 1){
    stop("Need consistent length ICD codes")
  }
  
  nchars
}


#' returns a statement of when the last ethnicity linkage occurred
#' @param lookback_months integer; number of months to look back for last
#'   linkage occurring
check_ethnicity_linkage <- function(lookback_months = 3) {
  con <- dbConnect(odbc(), 
                   Driver = "SQL Server", 
                   Server = Sys.getenv("DATA_LAKE_SERVER"), 
                   Database = Sys.getenv("RECENT_DEATHS_DATABASE"), 
                   Trusted_Connection = "True",
                   timeout = 120)
  
  
  
  deaths_table <- tbl(con, 
                      in_schema(
                        sql(Sys.getenv("RECENT_DEATHS_DATABASE")), 
                        sql(Sys.getenv("DEATHS_VIEW_RECENT")))
  )
  
  eth_lkp <- tbl(con, 
                 in_schema(
                   sql(Sys.getenv("RECENT_DEATHS_DATABASE")), 
                   sql(Sys.getenv("ETHNICITY_LINKED_DEATHS_VIEW_RECENT")))
  )
  
  x_months_ago <- Sys.Date() %m+% months(-lookback_months)
  
  last_linkage <- deaths_table |> 
    filter(DOR > x_months_ago) |> 
    left_join(eth_lkp,
              by = "Y124_Pseudo_LEDR_ID") |> 
    # filter(is.na(NEW_Ethnic_group)) |> 
    mutate(
      NEW_Ethnic_group = ifelse(
        is.na(NEW_Ethnic_group),
        NA_character_,
        "Linkage occurred"
      )
    ) |> 
    group_by(DOR, NEW_Ethnic_group) |> 
    summarise(records = n(),
              .groups = "drop") |> 
    arrange(desc(DOR)) |> 
    collect() |> 
    pivot_wider(names_from = NEW_Ethnic_group,
                values_from = records)
  
  required_fields <- c("NA", "Linkage occurred")
  missing_fields <- setdiff(
    required_fields,
    names(last_linkage)
  )
  
  if (length(missing_fields) > 0) last_linkage[missing_fields] <- NA
  
  last_linkage <- last_linkage |> 
    filter(
      !is.na(`Linkage occurred`)
    ) |> 
    slice(1) |> 
    pull(DOR)
  
  if (length(last_linkage) == 1) {
    print(paste("Ethnicity linkage process last occurred on",
                last_linkage))
  } else {
    print(paste("Ethnicity linkage process hasn't occurred in the last",
                lookback_months,
                "month(s)"))
  }
  
  dbDisconnect(con)
  return(invisible(last_linkage))
  
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

#' @description chect that split_chart exists in grouping_fields
split_chart_variables <- function(field, grouping_fields) {
  if (!(field %in% grouping_fields)) stop("chart splitting field isn't in the grouping fields created by facet_fields")
  
  return(field)
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


# Create output data files ------------------------------------------------


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


add_data_to_excel <- function(data, wb, output_filepath) {
  data_sheet <- as.character(unique(data$`Chart Group`))
  data_sheet <- gsub(" ", "_", data_sheet)
  
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
  
  sheet_references_lookup <- read.csv("data/excel_sheet_references.csv")
  sheet_references <- sheet_references_lookup %>% 
    tibble::deframe()
  
  if (geography == "england") {
    download_data <- read.csv(input_filepath) %>% 
      left_join(sheet_references_lookup, 
                by = "type") |> 
      dplyr::select(!c(type)) |> 
      dplyr::select(type = sheet_reference, everything()) |> 
      mutate(type = factor(type,
                           levels = sheet_references))
  } else if (geography == "region") {
    download_data <- read.csv(input_filepath) %>% 
      left_join(sheet_references_lookup, 
                by = "type") |> 
      dplyr::select(!c(type)) |> 
      dplyr::select(type = sheet_reference, everything()) |> 
      mutate(type = factor(case_when(type == "region" & RGN09CD == "E92000001" ~ "All Persons",
                                     TRUE ~ type),
                           levels = sheet_references),
             Chart_Name = case_when(type == "region" & RGN09CD != "E92000001" ~ "Region",
                                    type == "All Persons" & RGN09CD == "E92000001" ~ "All Persons",
                                    type == "pod" & Chart_Name == "" & Plot_Label == "All" ~ "All",
                                    TRUE ~ Chart_Name),
             Plot_Label = case_when(type == "region" & Plot_Label == "" ~ RGN09NM,
                                    TRUE ~ Plot_Label)) %>%
      filter(type == "utla" & RGN09CD != "E92000001"
             | type != "utla") %>%
      select(type, Chart_Name, Plot_Label, date, RGN09CD, RGN09NM, Sex,
             Ethnic_Group, Age_Group, name_of_cause, POD_out,
             Deprivation_Quintile, all_dths, exptd_dths, covid_dths, ucod_covid,
             ucod_disease)
    
    # accumulate data for regional download  
    eng_region_data <- download_data %>%
      filter(type %in% c("England","region")) %>%
      mutate(across(where(is.character), ~str_replace(.x, " All", "All")),
             Period_Start = as.Date(date) - 6,
             Period_End = as.Date(date)) %>%
      select(type:Deprivation_Quintile, Period_Start, Period_End,
             all_dths:ucod_disease,-date)
    
    
    chart_variables <- setdiff(names(download_data),c("date",
                                                      "all_dths",
                                                      "exptd_dths",
                                                      "covid_dths",
                                                      "ucod_covid",
                                                      "ucod_disease"))
    
    
    breakdown_data <- download_data %>%
      filter(!type %in% c("England","region")) %>%
      mutate(across(where(is.character), ~str_replace(.x, " All", "All"))) %>%
      mutate(date = as.Date(date)) %>%
      group_by(across(all_of(chart_variables))) %>% 
      summarise(Period_Start = range(date)[1] - 6,
                Period_End = range(date)[2],
                all_dths = sum(all_dths),
                exptd_dths = sum(exptd_dths),
                covid_dths = sum(covid_dths),
                ucod_covid = sum(ucod_covid),
                ucod_disease = sum(ucod_disease),
                .groups = "keep")
    
    download_data <- bind_rows(eng_region_data, breakdown_data) %>%
      mutate(across(where(is.Date), as.character),
             ratio = round_correct(all_dths / exptd_dths, 2))
    
    rm(eng_region_data, breakdown_data)
  }
  
  download_data <- split(download_data, download_data$type)
  
  download_data <- lapply(download_data,
                          remove_columns_with_all_or_nas_only,
                          geography = geography)
  
  if (geography == "region") {
    download_data <- download_data %>%
      purrr::map(~ select(., !c(`Registered Deaths`, `Expected Deaths`)))
  }
  
  # create the data within each tab of the workbook
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = "Contents")
  
  lapply(download_data, add_data_to_excel,
         wb = wb,
         output_filepath = output_filepath)
  
  # start developing the contents page
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
    
    sheet_name <- gsub(" ", "_", names(download_data)[[i]])
    
    # put hyperlinks into contents page to the appropriate tabs
    openxlsx::writeFormula(
      wb, sheet = "Contents",
      # will go in the cell below the "Contents:"
      startRow = contents_row$y + i,
      startCol = contents_row$x,
      x = openxlsx::makeHyperlinkString(
        sheet = sheet_name, row = 1, col = 1, 
        text = names(download_data)[[i]]
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
  # SD edit - issue with RDCOMClient
  #library(RDCOMClient, lib.loc = "C:/Users/sam.dunn/Documents/R/win-library/4.0")
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

