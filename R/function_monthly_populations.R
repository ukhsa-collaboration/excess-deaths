#' @description function returns monthly populations by geography, age group and
#'   sex for the time period selected
#' @details data generated from ONS mid-year population estimates stored on the
#'   Data Lake. Where the mid-year estimate doesn't exist for a time period the
#'   ONS projected population estimates are used
#' @param start_year earliest year for data generated
#' @param end_year latest year for data generated
#' @param use_2020_populations logical; if 2020 populations are available for
#'   both LTLA and LSOA then use them. We aren't going to use them initially
#'   because the 2020 estimates incorporate a lot of wave 1 deaths which we are
#'   trying to account for by doing mortality displacement analysis initially
#'   and then working out whether or not to include the population estimates
#' @param age_group_type string; "original", "nomis", "bespoke" are accepted
#'   inputs. If "bespoke" is used, then the bespoke_age_groups parameter needs
#'   to be provided
#' @param bespoke_age_groups tibble; two fields - Age and Age_Group. Age contains
#'   integers and Age_Group is a character class displaying the age group that
#'   that age is assigned to
#' @inheritParams get_baseline_deaths
get_denominators <- function(start_year = 2015L, end_year = year(Sys.Date()), 
                             eth_dep = FALSE,
                             age_filter = NULL,
                             use_2020_populations = FALSE,
                             age_group_type = NULL,
                             bespoke_age_groups = NULL) {
  
  source("R/utils.R")
  if (is.null(age_group_type)) {
    if (eth_dep == TRUE) {
      agegroup_lkp <- age_group_lkp(age_filter = age_filter, type = "nomis")
    } else {
      agegroup_lkp <- age_group_lkp(age_filter = age_filter, type = "original")
    }
  } else {
    agegroup_lkp <- age_group_lkp(age_filter = age_filter, 
                                  type = age_group_type,
                                  bespoke_age_groups = bespoke_age_groups)
  }
  
  
  utla_lkp <- utla_lookup()
  
  
  if (eth_dep == TRUE) {
    
    ethnicity_proportions <- read.csv("data/RGN09CD_eth_dep_ipf_estimated.csv") %>% 
      filter(Year %in% (start_year - 1):(end_year + 1)) %>% 
      rename(OfficialCode = RGN09CD,
             Period = Year,
             Population = population_estimates) %>% 
      mutate(Sex = case_when(
        Sex == "Males" ~ 1,
        Sex == "Females" ~ 2,
        TRUE ~ -99999),
        Sex = as.integer(Sex),
        Period = as.character(Period),
        year = as.Date(paste(Period, "7", "1",
                             sep = "-"), 
                       format = "%Y-%m-%d")) %>% 
      group_by(Period, year, OfficialCode, Sex, Age_Group, Deprivation_Quintile) %>% 
      mutate(proportions = Population / (sum(Population))) %>% 
      ungroup() %>% 
      dplyr::select(-Population)
  }
  
    
  # calculate latest year available in actual pops [vRes_CTRY09_SingleYear]
  con <- dbConnect(odbc(), 
                   Driver = "SQL Server", 
                   Server = Sys.getenv("DATA_LAKE_SERVER"), 
                   Database = Sys.getenv("POPULATIONS_DATABASE"), 
                   Trusted_Connection = "True",
                   timeout = 60)
  
  latest_pops_year <- tbl(con, 
                          in_schema(
                            sql(Sys.getenv("POPULATIONS_DATABASE")), 
                            sql(Sys.getenv("UTLA_FIVE_YR_POPULATIONS_TABLE_2020")))
                          ) %>%
    summarise(max_year = max(Period, na.rm = TRUE), 
              .groups = "keep") %>%
    collect() %>%
    pull(max_year) %>%
    as.integer()
  
  latest_lsoa_pops_year <- tbl(con, 
                               in_schema(
                                 sql(Sys.getenv("POPULATIONS_DATABASE")), 
                                 sql(Sys.getenv("LSOA11_SINGLE_YR_POPULATIONS_TABLE")))
                               ) %>% 
    summarise(max_year = max(Period, na.rm = TRUE), .groups = "keep") %>%
    collect() %>%
    pull(max_year) %>%
    as.integer()
  
  latest_pops_year <- min(latest_pops_year,
                          latest_lsoa_pops_year)
  
  # avoid using 2020 pop estimates because they contain deaths from the pandemic.
  # Our predictions are assuming the pandemic hasn't happened

  if (latest_pops_year == 2020 &
      use_2020_populations == FALSE) latest_pops_year <- 2019
  
  if (start_year - 1 < latest_pops_year) {
    if (eth_dep == TRUE) {
      utlas <- utla_lkp %>%
        pull(UTLAApr19CD)
      
      rgns <- utla_lkp %>%
        pull(RGN09CD) %>%
        unique()
      
      age_groups <- agegroup_lkp %>%
        pull(Age_Group) %>%
        unique()
      
      pops_table <- tbl(con, 
                        in_schema(
                          sql(Sys.getenv("POPULATIONS_DATABASE")), 
                          sql(Sys.getenv("LSOA11_SINGLE_YR_POPULATIONS_TABLE")))
      )
      area_lkup <- tbl(con, 
                       in_schema(
                         sql(Sys.getenv("LOOKUPS_DATABASE")), 
                         sql(Sys.getenv("LSOA_DEMOGRAPHICS_TABLE")))
      )
      
      populations <- pops_table %>%
        filter(Period >= (start_year - 1),
               Period <= latest_pops_year,
               OfficialCode %like% "E%",
               Sex != 4)  #remove persons
      
      if (!is.null(age_filter)) {
        ages <- convert_age_filter(age_filter)
        lower_age <- ages$lower_age
        upper_age <- ages$upper_age
        
        populations <- populations %>% 
          filter(between(Age, lower_age, upper_age))
      }
      
      populations <- populations %>% 
        left_join(area_lkup, by = c("OfficialCode" = "LSOA11CD")) %>%
        left_join(agegroup_lkp, by = "Age", copy = TRUE) %>%
        group_by(RGN09CD, IMD2019_Quintiles_LSOA11_England, Period, Sex, Age_Group) %>%
        summarise(Population = sum(Population, na.rm = TRUE),
                  .groups = "drop") %>% 
        rename(Deprivation_Quintile = IMD2019_Quintiles_LSOA11_England,
               OfficialCode = RGN09CD) %>%
        collect() %>%
        ungroup() %>%
        complete(OfficialCode = rgns,
                 Deprivation_Quintile = 1:5,
                 Period = as.character((start_year - 1):latest_pops_year),
                 Sex = 1:2,
                 Age_Group = age_groups,
                 fill = list(Population = 0)) %>%
        mutate(year = as.Date(paste(Period, "7", "1",
                                    sep = "-"), 
                              format = "%Y-%m-%d"))
    } else {
      utla_19_20_lkp <- tbl(con, 
                            in_schema(
                              sql(Sys.getenv("LOOKUPS_DATABASE")), 
                              sql(Sys.getenv("UTLA19_LKP_TABLE")))
                            ) %>% 
        distinct(UTLA19CD, UTLA20CD) %>% 
        rename(OfficialCode = UTLA19CD)
      pops_table <- tbl(con, 
                        in_schema(
                          sql(Sys.getenv("POPULATIONS_DATABASE")), 
                          sql(Sys.getenv("UTLA19_SINGLE_YR_POPULATIONS_TABLE_2020")))
                        ) %>% 
        rename(UTLA20CD = OfficialCode) %>% 
        left_join(utla_19_20_lkp, by = "UTLA20CD")

      populations <- pops_table %>%
        filter(Period >= (start_year - 1),
               Period <= latest_pops_year,
               Sex != 4)  #remove persons
      
      if (!is.null(age_filter)) {
        ages <- convert_age_filter(age_filter)
        lower_age <- ages$lower_age
        upper_age <- ages$upper_age
        
        populations <- populations %>% 
          filter(between(Age, lower_age, upper_age))
      }
      
      populations <- populations %>%
        left_join(agegroup_lkp, by = "Age", copy = TRUE) %>%
        group_by(OfficialCode, Period, Sex, Age_Group) %>%
        summarise(Population = sum(Population, na.rm = TRUE),
                  .groups = "drop") %>% 
        collect() %>%
        ungroup() %>%
        mutate(year = as.Date(paste(Period, "7", "1",
                                    sep = "-"), 
                              format = "%Y-%m-%d"))
    }
    
  } else {
    populations <- tibble()
  }
  
  # calculate england total from projections broken down by:
  
  if ((end_year + 1) > latest_pops_year) {
    
    if (nrow(populations) > 0) {
      year_filter <- max(latest_pops_year, start_year)
    } else {
      year_filter <- start_year - 2
    }
    
    projections_table <- tbl(con, 
                             in_schema(
                               sql(Sys.getenv("POPULATIONS_DATABASE")),
                               sql(Sys.getenv("LTLA13_POPULATION_PROJECTIONS_TABLE")))
    )
    
    lkup <- tbl(con, 
                in_schema(
                  sql(Sys.getenv("LOOKUPS_DATABASE")), 
                  sql(Sys.getenv("LTLA_LKP_TABLE")))
    )
    
    projections <- projections_table %>%
      filter(Period > year_filter,
             Period <= end_year + 1,
             Sex != 4) #remove persons
    
    if (!is.null(age_filter)) {
      ages <- convert_age_filter(age_filter)
      lower_age <- ages$lower_age
      upper_age <- ages$upper_age
      
      projections <- projections %>% 
        filter(between(Age, lower_age, upper_age))
    }
    
    projections <- projections %>% 
      left_join(agegroup_lkp, by = "Age", copy = TRUE) %>%
      left_join(lkup, by = c("OfficialCode" = "LTLA13CD")) %>%
      group_by(UTLA19CD, Period, Sex, Age_Group) %>% 
      filter(ONSPubDate == min(ONSPubDate, na.rm = TRUE)) %>% 
      summarise(Population = sum(Population, na.rm = TRUE),
                .groups = "drop") %>%
      rename(OfficialCode = UTLA19CD) %>%
      collect() %>% 
      mutate(year = as.Date(paste(Period, "7", "1",
                                  sep = "-"), 
                            format = "%Y-%m-%d"))
    
    if (eth_dep == TRUE) {
      # we don't have a projections file for LSOA populations, 
      # so we use the latest time period from the mid-year populations estimates 
      # to calculate proportions that we then apply to the non-deprivation denominators
      
      utlas <- utla_lkp %>%
        pull(UTLAApr19CD)
      

      rgns <- utla_lkp %>%
        pull(RGN09CD) %>%
        unique()

      age_groups <- age_group_lkp(age_filter = age_filter,
                                  type = "nomis") %>%
        pull(Age_Group) %>%
        unique()
      
      pops_table <- tbl(con, 
                        in_schema(
                          sql(Sys.getenv("POPULATIONS_DATABASE")), 
                          sql(Sys.getenv("LSOA11_SINGLE_YR_POPULATIONS_TABLE")))
                        )
      
      area_lkup <- tbl(con, 
                       in_schema(
                         sql(Sys.getenv("LOOKUPS_DATABASE")), 
                         sql(Sys.getenv("LSOA_DEMOGRAPHICS_TABLE")))
                       )
      
      periods <- tibble(Period = as.character((year_filter + 1):(end_year + 1)))
      
      deprivation_proportions <- pops_table %>%
        filter(Period == latest_pops_year,
               OfficialCode %like% "E%",
               Sex != 4)  #remove persons
      
      if (!is.null(age_filter)) {
        ages <- convert_age_filter(age_filter)
        lower_age <- ages$lower_age
        upper_age <- ages$upper_age
        
        deprivation_proportions <- deprivation_proportions %>% 
          filter(between(Age, lower_age, upper_age))
      }
      
      deprivation_proportions <- deprivation_proportions %>% 
        left_join(area_lkup, by = c("OfficialCode" = "LSOA11CD")) %>%
        left_join(agegroup_lkp, by = "Age", copy = TRUE) %>%
        group_by(RGN09CD, IMD2019_Quintiles_LSOA11_England, Sex, Age_Group) %>%
        summarise(Population = sum(Population, na.rm = TRUE),
                  .groups = "drop") %>% 
        rename(Deprivation_Quintile = IMD2019_Quintiles_LSOA11_England,
               OfficialCode = RGN09CD) %>%
        collect() %>%
        ungroup() %>%
        complete(OfficialCode = rgns,
                 Deprivation_Quintile = 1:5,
                 Sex = 1:2,
                 Age_Group = age_groups,
                 fill = list(Population = 0)) %>%
        merge(periods, all.x = TRUE, all.y = TRUE) %>% # repeat pops for each year we don't have data for
        mutate(year = as.Date(paste(Period, "7", "1",
                                    sep = "-"), 
                              format = "%Y-%m-%d")) %>% 
        group_by(OfficialCode, Sex, Age_Group, Period, year) %>% 
        mutate(proportions = Population / sum(Population)) %>% 
        ungroup() %>% 
        dplyr::select(-Population)
      
      projections <- projections %>% 
        left_join(utla_lkp, by = c("OfficialCode" = "UTLAApr19CD")) %>% 
        group_by(RGN09CD, Sex, Age_Group, Period, year) %>% 
        summarise(Population = sum(Population), 
                  .groups = "drop") %>% 
        rename(OfficialCode = RGN09CD) %>% 
        left_join(deprivation_proportions, by = c("OfficialCode", "Sex", "Age_Group", "Period", "year")) %>% 
        mutate(Population = Population * proportions) %>% 
        dplyr::select(-proportions)
                    
    } 
    
    
    combined_pops <- populations %>%
      bind_rows(projections) %>%
      ungroup()
    
    
  } else {
    combined_pops <- populations
  }
  
  dbDisconnect(con)
    
  # apply the ethnicity proportions to the population
  if (eth_dep == TRUE) {
      join_groups <- c("OfficialCode", "Sex", "Age_Group", "Deprivation_Quintile", "year", "Period")
    
      combined_pops <- combined_pops %>% 
        left_join(ethnicity_proportions, 
                  by = join_groups) %>% 
        mutate(Population = Population * proportions) %>% 
        dplyr::select(-proportions)
  }
  
  denominators <- combined_pops %>%
    dplyr::select(-Population, -year) %>%
    mutate(month = 1L,
           Period = as.integer(Period))
  
  if (eth_dep == TRUE) {
    denominators <- denominators %>%
      complete(month = seq(from = 1L,
                           to = 12L),
               Period,
               OfficialCode,
               Sex, 
               Age_Group,
               Ethnic_Group,
               Deprivation_Quintile)
      
   
    
  } else {
    denominators <- denominators %>%
      complete(month = seq(from = 1L,
                           to = 12L),
               Period,
               OfficialCode,
               Sex, 
               Age_Group)
    
    
  }
  
  denominators <- denominators %>%
    mutate(earlier_year = case_when(
      month < 7L ~ Period - 1L,
      TRUE ~ Period),
      later_year = case_when(
        month < 7L ~ Period,
        TRUE ~ Period + 1L),
      start_month = as.Date(paste(Period, month, "1",
                                  sep = "-"), format = "%Y-%m-%d"),
      end_month = start_month %m+% months(1),
      start_year = as.Date(paste(Period, "1", "1",
                                 sep = "-"), format = "%Y-%m-%d"),
      end_year = start_year + years(1),
      start_mid_yr_est = as.Date(paste(earlier_year, "7", "1",
                                       sep = "-"), format = "%Y-%m-%d"),
      mid_point_month = start_month - ((start_month - end_month) / 2),
      days_in_month = end_month - start_month,
      days_in_year = end_year - start_year,
      diff_mid_point_mid_yr_est = mid_point_month - start_mid_yr_est)
  
  if (eth_dep == TRUE){
    denominators <- denominators %>%
      left_join(combined_pops[, c("OfficialCode", "Age_Group", "Sex", "Ethnic_Group", "Deprivation_Quintile", "year", "Population")], 
                by = c("OfficialCode", "Age_Group", "Sex", "Ethnic_Group", "Deprivation_Quintile", "start_mid_yr_est" = "year"))  
    
    
    
  } else {
    denominators <- denominators %>%
      left_join(combined_pops[, c("OfficialCode", "Age_Group", "Sex", "year", "Population")], 
                by = c("OfficialCode", "Age_Group", "Sex", "start_mid_yr_est" = "year"))
    
    
  }
  
  denominators <- denominators %>%
    rename(population1 = Population) %>%
    mutate(later_year_join = start_mid_yr_est + years(1))
  
  if (eth_dep == TRUE) {
    denominators <- denominators %>%
      left_join(combined_pops[, c("OfficialCode", "Age_Group", "Sex", "Ethnic_Group", "Deprivation_Quintile", "year", "Population")], 
                by = c("OfficialCode", "Age_Group", "Sex", "Ethnic_Group", "Deprivation_Quintile", "later_year_join" = "year"))
    
    
  } else {
    denominators <- denominators %>%
      left_join(combined_pops[, c("OfficialCode", "Age_Group", "Sex", "year", "Population")], 
                by = c("OfficialCode", "Age_Group", "Sex", "later_year_join" = "year"))
    
  }
  
  denominators <- denominators %>%
    rename(population2 = Population) %>%
    dplyr::select(-later_year_join) %>%
    mutate(Cal1 = ((population2 - population1) * (as.integer(diff_mid_point_mid_yr_est) / as.integer(days_in_year))) + population1,
           Cal2 = as.integer(days_in_month) / as.integer(days_in_year),
           monthly_population = Cal1 * Cal2,
           denominator = monthly_population / as.integer(days_in_month)) #### Divide by days in month to get a daily pop per month
  
  if (eth_dep == TRUE) {
    denominators <- denominators %>%
      dplyr::select(OfficialCode, Sex, Age_Group, Ethnic_Group, Deprivation_Quintile, month = start_month, denominator = monthly_population)
    
    
  } else {
    denominators <- denominators %>%
      dplyr::select(OfficialCode, Sex, Age_Group, month = start_month, denominator = monthly_population)
    
    
  }
  
  denominators <- denominators %>%
    filter(!is.na(denominator),
           year(month) %in% start_year:end_year) %>%
    ungroup()
  return(denominators)
  
}

