pre_processed_death_checks <- function(deaths_data, utla_lkp, holidays, eth_dep, 
                                       deaths_reallocated,
                                       all_pod = FALSE) {
  
  ### data checks ###
  
  date_range <- range(deaths_data$Reg_Date)
  
  all_dates <- data.frame(dates = seq(from = date_range[1],
                                      to = date_range[2],
                                      by = "days")) %>%
    adjust_first_date(dates, holidays) %>%
    pull(dates)
  
  all_age_groups <- length(unique(deaths_data$Age_Group))
  
  expected_records <- length(all_dates) *
    2 * #sex
    all_age_groups
  
  if (eth_dep == TRUE) {
    expected_records <- expected_records *
      length(unique(deaths_data$Ethnic_Group)) *
      length(unique(utla_lkp$RGN09CD)) *
      length(1:5) #deprivation
    
    deaths_data %>%
      chain_start() %>%
      verify(has_all_names("RGN09CD", "Ethnic_Group", "Deprivation_Quintile", "Sex", "Age_Group", "Reg_Date", "deaths_total")) %>%
      verify(nrow(.) == expected_records) %>%
      assert_rows(col_concat, is_uniq, RGN09CD, Ethnic_Group, Deprivation_Quintile, Sex, Age_Group, Reg_Date) %>%
      assert(not_na, everything()) %>%
      assert(within_bounds(0, Inf), deaths_total) %>%
      chain_end()
  
  } else {
    
    expected_records <- expected_records *
      length(unique(utla_lkp$UTLAApr19CD))
    
    if (all_pod == TRUE) {
      pods <- pod_lookup() %>%
        pull(pod_filter) %>%
        unique()
      expected_records <- expected_records *
        length(pods)
      
      deaths_data %>%
        chain_start() %>%
        verify(has_all_names("UTLAApr19CD", "POD_out", "Sex", "Age_Group", "Reg_Date", "deaths_total")) %>%
        verify(nrow(.) == expected_records) %>%
        assert_rows(col_concat, is_uniq, UTLAApr19CD, POD_out, Sex, Age_Group, Reg_Date) %>%
        assert(not_na, everything()) %>%
        assert(within_bounds(0, Inf), deaths_total) %>%
        chain_end()
    } else {
      deaths_data %>%
        chain_start() %>%
        verify(has_all_names("UTLAApr19CD", "Sex", "Age_Group", "Reg_Date", "deaths_total")) %>%
        verify(nrow(.) == expected_records) %>%
        assert_rows(col_concat, is_uniq, UTLAApr19CD, Sex, Age_Group, Reg_Date) %>%
        assert(not_na, everything()) %>%
        assert(within_bounds(0, Inf), deaths_total) %>%
        chain_end()
    }
    
  }
  

  
  
  
  total_deaths <- deaths_data %>%
    summarise(deaths_total = sum(deaths_total)) %>%
    pull() # for later checks
  
  if (deaths_reallocated == TRUE) {
    dates_around_holidays <- c(holidays, (holidays - 1), (holidays + 1))
    
    total_deaths_around_hols_weekends <- deaths_data %>%
      filter(wday(Reg_Date) %in% c(1, 2, 6, 7) |
               Reg_Date %in% dates_around_holidays) %>%
      summarise(deaths_total = sum(deaths_total)) %>%
      pull() # for later checks
    
    check_results <- list(expected_records = expected_records,
                          total_deaths = total_deaths,
                          total_deaths_around_hols_weekends = total_deaths_around_hols_weekends)
  } else if (deaths_reallocated == FALSE) {
    check_results <- list(expected_records = expected_records,
                          total_deaths = total_deaths)
  }
  
  return(check_results)
}

post_processed_death_checks <- function(deaths_data, holidays, total_deaths, 
                                        total_deaths_around_hols_weekends = NULL, eth_dep, 
                                        deaths_reallocated, deaths_field, 
                                        days_reallocated = deaths_reallocated) {

  if (deaths_reallocated == TRUE) {
    
    if (days_reallocated == TRUE) {
      test_days <- in_set(2, 3, 4, 5, 6)
    } else {
      test_days <- in_set(1, 2, 3, 4, 5, 6, 7)
    }
    
    dates_around_holidays <- c(holidays, (holidays - 1), (holidays + 1))
    deaths_data <- deaths_data %>%
      mutate(weekday = wday(date),
             around_hol_we_deaths = case_when(
               wday(date) %in% c(1, 2, 6, 7) |
                 date %in% dates_around_holidays ~ {{ deaths_field }},
               TRUE ~ 0),
             hol_deaths = case_when(
               wday(date) %in% c(1, 7) |
                 date %in% holidays ~ {{ deaths_field }},
               TRUE ~ 0)) %>%
      chain_start() %>%
      verify(round_correct(sum(around_hol_we_deaths), 8) == 
               round_correct(total_deaths_around_hols_weekends, 8)) %>%
      assert(test_days, weekday) %>%
      verify(sum(hol_deaths) == 0) %>%
      chain_end()
  } else if (deaths_reallocated == FALSE) {
    deaths_data <- deaths_data %>%
      mutate(weekday = wday(date)) %>%
      assert(in_set(1, 2, 3, 4, 5, 6, 7), weekday)
  }
  deaths_data %>%
    chain_start() %>%
    assert(not_na, everything()) %>%
    verify(round_correct(sum({{ deaths_field }}), 8) == round_correct(total_deaths, 8)) %>%
    assert(in_set(0, 1), Sex) %>%
    assert(is.factor, Sex, Age_Group) %>%
    chain_end()
  
  if (eth_dep == TRUE) deaths_data %>%
    assert(is.factor, Ethnic_Group) %>%
    assert(is.factor, Deprivation_Quintile)
  
  return(TRUE)
}

pre_processed_denominators_checks <- function(denominators, start_year, end_year, 
                                              utla_lkp, eth_dep, age_filter) {
  
  if (eth_dep == TRUE) {
    expected_records_denominators <- length(start_year:end_year) *
      12 * #months
      2 * #sex
      length(unique(age_group_lkp(age_filter = age_filter)$Age_Group)) *
      length(unique(utla_lkp$RGN09CD)) *
      length(unique(denominators$Ethnic_Group)) *
      length(1:5) #deprivation
    
    denominators %>%
      chain_start() %>%
      verify(has_all_names("OfficialCode", "Ethnic_Group", "Deprivation_Quintile", "Sex", "Age_Group", "month", "denominator")) %>%
      verify(nrow(.) == expected_records_denominators) %>%
      assert_rows(col_concat, is_uniq, OfficialCode, Ethnic_Group, Deprivation_Quintile, Sex, Age_Group, month) %>%
      assert(not_na, everything()) %>%
      assert(within_bounds(0, Inf), denominator) %>%
      chain_end()
    
    
  } else {
    expected_records_denominators <- length(start_year:end_year) *
      12 * #months
      2 * #sex
      length(unique(age_group_lkp(age_filter = age_filter)$Age_Group)) *
      length(unique(utla_lkp$UTLAApr19CD))
    
    denominators %>%
      chain_start() %>%
      verify(has_all_names("OfficialCode", "Sex", "Age_Group", "month", "denominator")) %>%
      verify(nrow(.) == expected_records_denominators) %>%
      assert_rows(col_concat, is_uniq, OfficialCode, Sex, Age_Group, month) %>%
      assert(not_na, everything()) %>%
      assert(within_bounds(0, Inf), denominator) %>%
      chain_end()
  }
  
  
  total_denominators <- sum(denominators$denominator) # for later checks
  check_results <- list(total_denominators = total_denominators)
  return(check_results)
}

post_processed_denominators_checks <- function(denominators, total_denominators, eth_dep) {
  
  denominators %>%
    verify(round_correct(sum(denominator), 8) == round_correct(total_denominators, 8)) %>%
    assert(in_set(0, 1), Sex) %>%
    assert(is.factor, Sex, Age_Group)
  
  if (eth_dep == TRUE) denominators %>%
    assert(is.factor, Ethnic_Group) %>%
    assert(is.factor, Deprivation_Quintile)
  return(TRUE)
}
