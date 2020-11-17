#' @description function returns daily deaths by geography, age group and sex
#'   between 2015 and 2018 (or 2019 depending on whether include_2019 = TRUE)
#' @details this function requires access to the BirthsDeaths database on the
#'   Data Lake. When the arguments for deprivation or ethnicity are TRUE, the
#'   geography will be region. Otherwise, the region will be UTLA (April 2019)
#' @param ucod string, ICD10 codes to filter deaths in the underlying cause of
#'   death field. Allowable strings are a single value, a character vector, or a
#'   single value with a '%' at the end, which is used in the same way as a '%'
#'   in SQL (ie, LIKE 'E%)
#' @param btw_ucod string, ICD10 codes to filter deaths in the underlying cause
#'   of death field. Length of string must be two, with the second value being
#'   'higher' alphabetically than the first. The first value is used to filter
#'   the UCOD field as a lower limit (greater than or equal to) and the second
#'   value filters the UCOD as an upper limit (less than)
#' @param ucods string, ICD10 codes to filter deaths in the underlying cause of
#'   death field. Vector can be any length and is used to filter the UCOD field
#'   with an %in% function (on the first 3 characters of the UCODs field)
#' @param cod string, ICD10 codes used to filter the UCOD and the 15 COD fields
#'   to identify a mention of a cause. It filters these fields with an %in%
#'   function (on the first 3 characters of the UCODs field)
#' @param pod string, vector of length up to five describing the place of death.
#'   Each item must be one of the following; "home", "care home", "hospital",
#'   "hospice", "other". Default is NULL, which represents all places
#' @param deprivation logical, include deprivation quintile as an additional
#'   field in the outputs. All place of death and cause of death filters will
#'   work when this argument is TRUE
#' @param ethnicity logical, include the 5 ethnic groups as an additional field
#'   in the outputs. If TRUE, this reads a file stored on the Data Lake secure
#'   file share for the daily deaths. Filters for place of death and cause of
#'   death will not work when this argument is TRUE.
#' @param include_2019 logical, whether to include data for 2019 in the outputs
#'   too. When TRUE, this uses the MBIS monthly deaths database, so users would
#'   need access to that. This argument is ignored when ethnicity = TRUE
#' @param include_ethnicity_uplift logical, when TRUE, the proportion of daily
#'   deaths in each age group, sex, ethnic group and region are applied to the
#'   same time period of deaths from the BirthsDeaths database
#' @param age_filter numeric length 2; lower and upper limits (inclusive) for
#'   age filter. Where limit is the extent of the range use NA, eg, c(75, NA) is
#'   equivalent to "75+"
#'
#'   
get_baseline_deaths <- function(ucod = NULL, btw_ucod = NULL, ucods = NULL, cod = NULL,
                                pod = NULL, deprivation = FALSE, ethnicity = FALSE, 
                                include_2019 = FALSE, include_ethnicity_uplift = FALSE, 
                                age_filter = NULL, age_group_type = "original") {

  # check pod
  if (!is.null(pod)) {
    if (length(setdiff(pod, c("home", "care home", "hospital", "hospice", "other"))) > 0) {
      stop("pod not defined properly")
    }
  }
  
  if ((deprivation == TRUE & ethnicity == TRUE) |
      age_group_type == "nomis") {
    agegroup_lkp <- age_group_lkp(age_filter = age_filter, type = "nomis")  
  } else {
    agegroup_lkp <- age_group_lkp(age_filter = age_filter, type = "original")
  }
  
  
  ## ETHNICITY CURRENTLY PROVIDED IN A FILE SO FOLLOWING CODE SUPERCEDES FUTURE ETHNICITY CODING FOR NOW ###
  if (ethnicity == TRUE) {
    utla_lkp <- utla_lookup()
    
    rgn_lkp <- utla_lkp %>% 
      distinct(RGN09NM, RGN09CD)
    
    ethnic_groups <- ethnic_groups_lookup() %>%
      dplyr::select(Ethnic_Group, Ethnicity_Broad) %>%
      unique()
    
    if (deprivation == FALSE) {
      filepath <- Sys.getenv("ETHNICITY_BASELINE_DEATHS")
      lower_geogs_deaths_table <- read_excel(filepath,
                                             sheet = "Deaths_byRegionBroadEthnicgroup")
      
      
      
      if (include_2019 == TRUE) {
        filepath_2019 <- Sys.getenv("ETHNICITY_BASELINE_DEATHS_2019")
        lower_geogs_deaths_table_2019 <- read_excel(filepath_2019,
                                                    sheet = "Deaths_byRegionBroadEthnicgroup",
                                                    range = "A2:F52676") %>% 
          left_join(rgn_lkp, by = c("Region" = "RGN09CD")) %>% 
          dplyr::select(Reg_Date, Region = RGN09NM, Age_group, Sex, Ethnicity_Broad, Deaths_total)
        
        lower_geogs_deaths_table <- bind_rows(lower_geogs_deaths_table,
                                              lower_geogs_deaths_table_2019)
        
        end_date <- as.Date("2019-12-31")
      } else {
        end_date <- as.Date("2018-12-31")
      }
      
      lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
        mutate(Reg_Date = as.Date(as.character(Reg_Date), format = "%Y%m%d"),
               Sex = as.integer(Sex),
               Region = case_when(
                 Region == "Yorkshire and the Humber" ~ "Yorkshire and The Humber",
                 TRUE ~ Region
               ),
               Age_group = case_when(
                 Age_group == "<15" ~ "0-14",
                 TRUE ~ Age_group
               )) %>%
        left_join(ethnic_groups, by = "Ethnicity_Broad") %>%
        filter(Sex %in% c(1, 2),
               year(Reg_Date) >= 2015) %>%
        left_join(rgn_lkp, by = c("Region" = "RGN09NM")) %>% 
        dplyr::select(RGN09CD, Ethnic_Group, Reg_Date,
                      Age_Group = Age_group, Sex,
                      deaths_total = Deaths_total) %>%
        complete(RGN09CD = unique(rgn_lkp$RGN09CD),
                 Ethnic_Group = ethnic_groups$Ethnic_Group,
                 Reg_Date = seq.Date(from = as.Date("2015-01-01"),
                                     to = end_date,
                                     by = "days"),
                 Age_Group = unique(agegroup_lkp$Age_Group),
                 Sex,
                 fill = list(deaths_total = 0)) %>%
        arrange(RGN09CD, Ethnic_Group, Sex, Age_Group, Reg_Date)
      
      if (include_ethnicity_uplift == TRUE) {
        daily_death_totals <- get_baseline_deaths(include_2019 = TRUE) %>% 
          left_join(utla_lkp, by = "UTLAApr19CD") %>% 
          group_by(RGN09CD, Reg_Date, Age_Group, Sex) %>% 
          summarise(deaths_total = sum(deaths_total), .groups = "drop")
        
        # E12000001, 0-14, 1: no deaths for 17 days either side of 2015-10-19 in ETHS data
        daily_proportions <- lower_geogs_deaths_table %>% 
          group_by(RGN09CD, Age_Group, Sex, Ethnic_Group) %>% 
          mutate(average_deaths_over_period = slide_index_dbl(.x = deaths_total,
                                                              .i = Reg_Date, 
                                                              .f = mean, 
                                                              .before = 17, 
                                                              .after = 17,
                                                              .complete = FALSE)) %>% 
          group_by(Reg_Date, RGN09CD, Age_Group, Sex) %>% 
          mutate(proportion = average_deaths_over_period / sum(average_deaths_over_period)) %>% 
          ungroup() %>% 
          dplyr::select(-c(deaths_total, average_deaths_over_period))
        
        lower_geogs_deaths_table <- daily_death_totals %>%
          left_join(daily_proportions, by = c("RGN09CD", "Reg_Date", "Age_Group", "Sex")) %>%
          mutate(deaths_total = deaths_total * proportion) %>% 
          dplyr::select(-proportion) %>%
          complete(RGN09CD = unique(rgn_lkp$RGN09CD),
                   Ethnic_Group = ethnic_groups$Ethnic_Group,
                   Reg_Date = seq.Date(from = as.Date("2015-01-01"),
                                       to = end_date,
                                       by = "days"),
                   Age_Group = unique(agegroup_lkp$Age_Group),
                   Sex,
                   fill = list(deaths_total = 0)) %>%
          arrange(RGN09CD, Ethnic_Group, Sex, Age_Group, Reg_Date)
      }
      
      
      ethnicity_baseline <- get_denominators(start_year = 2015,
                                             end_year = year(end_date),
                                             ethnicity = TRUE) %>%
        rename(RGN09CD = OfficialCode,
               deaths_total = denominator) %>%
        dplyr::select(-month)
      
      ethnicity_proportions <- calculate_ethnicity_proportions(data = ethnicity_baseline,
                                                               ethnicity_field = Ethnic_Group,
                                                               region_field = RGN09CD,
                                                               age_field = Age_Group,
                                                               sex_field = Sex,
                                                               deaths_field = deaths_total)
      
      lower_geogs_deaths_table <- ethnicity_not_stated_adjustment(data = lower_geogs_deaths_table,
                                                                  proportions = ethnicity_proportions,
                                                                  ethnicity_field = Ethnic_Group,
                                                                  deaths_field = deaths_total,
                                                                  region_field = RGN09CD,
                                                                  age_field = Age_Group,
                                                                  sex_field = Sex,
                                                                  date_field = Reg_Date)
    } else if (deprivation == TRUE) {

# deprivation and ethnicity -----------------------------------------------
      filepath <- Sys.getenv("ETHNICITY_DEPRIVATION_BASELINE_DEATHS")
      lower_geogs_deaths_table <- read_excel(filepath,
                                             sheet = "Deaths_DepQuintileRegionEthni",
                                             range = "R2C1:R667804C7")
      
      lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
        mutate(Reg_Date = as.Date(as.character(Reg_Date), format = "%Y%m%d"),
               Sex = as.integer(Sex)) %>%
        left_join(ethnic_groups, by = "Ethnicity_Broad") %>%
        filter(Sex %in% c(1, 2),
               year(Reg_Date) >= 2015,
               Deprivation_Quintile != "NULL") %>%
        mutate(Deprivation_Quintile = as.integer(Deprivation_Quintile)) %>% 
        dplyr::select(RGN09CD = Region, Ethnic_Group, Deprivation_Quintile,
                      Reg_Date,
                      Age_Group = Age_group, Sex,
                      deaths_total = Deaths_total)
      
      if (include_2019 == TRUE) {
        end_date <- as.Date("2019-12-31")
      } else {
        lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
          filter(Reg_Date < as.Date("2019-01-01"))
        end_date <- as.Date("2018-12-31")
      }
      
      lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
        complete(RGN09CD = unique(rgn_lkp$RGN09CD),
                 Ethnic_Group = ethnic_groups$Ethnic_Group,
                 Deprivation_Quintile = 1:5,
                 Reg_Date = seq.Date(from = as.Date("2015-01-01"),
                                     to = end_date,
                                     by = "days"),
                 Age_Group = unique(agegroup_lkp$Age_Group),
                 Sex,
                 fill = list(deaths_total = 0)) %>%
        arrange(RGN09CD, Ethnic_Group, Deprivation_Quintile, Sex, Age_Group, Reg_Date)
      
      if (include_ethnicity_uplift == TRUE) {
        daily_death_totals <- get_baseline_deaths(include_2019 = TRUE,
                                                  age_group_type = "nomis") %>% 
          left_join(utla_lkp, by = "UTLAApr19CD") %>% 
          group_by(RGN09CD, Reg_Date, Age_Group, Sex) %>% 
          summarise(deaths_total = sum(deaths_total), .groups = "drop")
        
        daily_proportions <- lower_geogs_deaths_table %>% 
          group_by(RGN09CD, Age_Group, Sex, Ethnic_Group, Deprivation_Quintile) %>% 
          mutate(average_deaths_over_period = slide_index_dbl(.x = deaths_total,
                                                              .i = Reg_Date, 
                                                              .f = mean, 
                                                              .before = 11, 
                                                              .after = 11,
                                                              .complete = FALSE)) %>% 
          group_by(Reg_Date, RGN09CD, Age_Group, Sex) %>% 
          mutate(proportion = average_deaths_over_period / sum(average_deaths_over_period)) %>% 
          ungroup() %>% 
          dplyr::select(-c(deaths_total, average_deaths_over_period))
        
        lower_geogs_deaths_table <- daily_death_totals %>%
          left_join(daily_proportions, by = c("RGN09CD", "Reg_Date", "Age_Group", "Sex")) %>%
          mutate(deaths_total = deaths_total * proportion) %>% 
          dplyr::select(-proportion) %>%
          complete(RGN09CD = unique(rgn_lkp$RGN09CD),
                   Ethnic_Group = ethnic_groups$Ethnic_Group,
                   Deprivation_Quintile = 1:5,
                   Reg_Date = seq.Date(from = as.Date("2015-01-01"),
                                       to = end_date,
                                       by = "days"),
                   Age_Group = unique(agegroup_lkp$Age_Group),
                   Sex,
                   fill = list(deaths_total = 0)) %>%
          arrange(RGN09CD, Ethnic_Group, Sex, Age_Group, Reg_Date)
      }
      
      ethnicity_deprivation_baseline <- get_denominators(start_year = 2015,
                                                         end_year = year(end_date),
                                                         ethnicity = TRUE, 
                                                         deprivation = TRUE) %>%
        rename(RGN09CD = OfficialCode,
               deaths_total = denominator) %>%
        dplyr::select(-month)
      
      ethnicity_proportions <- calculate_ethnicity_proportions(data = ethnicity_deprivation_baseline,
                                                               ethnicity_field = Ethnic_Group,
                                                               region_field = RGN09CD,
                                                               age_field = Age_Group,
                                                               sex_field = Sex,
                                                               deaths_field = deaths_total,
                                                               include_deprivation = TRUE)
      
      lower_geogs_deaths_table <- ethnicity_not_stated_adjustment(data = lower_geogs_deaths_table,
                                                                  proportions = ethnicity_proportions,
                                                                  ethnicity_field = Ethnic_Group,
                                                                  deaths_field = deaths_total,
                                                                  region_field = RGN09CD,
                                                                  age_field = Age_Group,
                                                                  sex_field = Sex,
                                                                  date_field = Reg_Date,
                                                                  include_deprivation = TRUE)
    }
    
    
    return(lower_geogs_deaths_table)
  }
  
  con <- dbConnect(odbc(), 
                   Driver = "SQL Server", 
                   Server = Sys.getenv("DATA_LAKE_SERVER"), 
                   Database = Sys.getenv("BIRTHS_DEATHS_DATABASE"), 
                   Trusted_Connection = "True",
                   timeout = 120)
  
  source_table <- tbl(con, in_schema(Sys.getenv("BIRTHS_DEATHS_DATABASE"), Sys.getenv("DEATHS_VIEW_BEFORE_2019")))
  
  
  source("R/utils.R")
  
  
  lower_geogs_deaths_table <- source_table  %>%
    filter(xYear >= 2015 & xYear <= 2018,
           (GOR_Resi_9R %like% "E%" | (GOR_Resi >= "A" & GOR_Resi <= "K")),
           Sex %in% c(1, 2)) 
  
  if (!is.null(age_filter)) {
    ages <- convert_age_filter(age_filter)
    lower_age <- ages$lower_age
    upper_age <- ages$upper_age
    
    lower_geogs_deaths_table <- lower_geogs_deaths_table %>% 
      filter(between(xAGE_Year, lower_age, upper_age))
  }
  
  if (!is.null(ucod)) {
    if (length(ucod) == 1) {
      if (grepl("%", ucod)){
        lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
          filter(UCOD %like% ucod)
      } else {
        lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
          filter(UCOD == ucod)
      }
    } else {
      lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
        filter(UCOD %in% ucod)
    }
  } else if (!is.null(ucods)) {
    lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
      filter(substr(UCOD, 1, 3) %in% ucods)
  } else if (!is.null(btw_ucod)) {
    lower_code <- btw_ucod[1]
    upper_code <- btw_ucod[2]
    lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
      filter(UCOD >= lower_code,
             UCOD < upper_code)
  } else if (!is.null(cod)) {
    lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
      filter(substr(UCOD, 1, 3) %in% cod |
               substr(COD_1, 1, 3) %in% cod |
               substr(COD_2, 1, 3) %in% cod |
               substr(COD_3, 1, 3) %in% cod |
               substr(COD_4, 1, 3) %in% cod |
               substr(COD_5, 1, 3) %in% cod |
               substr(COD_6, 1, 3) %in% cod |
               substr(COD_7, 1, 3) %in% cod |
               substr(COD_8, 1, 3) %in% cod |
               substr(COD_9, 1, 3) %in% cod |
               substr(COD_10, 1, 3) %in% cod |
               substr(COD_11, 1, 3) %in% cod |
               substr(COD_12, 1, 3) %in% cod |
               substr(COD_13, 1, 3) %in% cod |
               substr(COD_14, 1, 3) %in% cod |
               substr(COD_15, 1, 3) %in% cod)
  }
  
  if (!is.null(pod)) {
    pod_table <- tbl(con, in_schema(Sys.getenv("BIRTHS_DEATHS_DATABASE"), Sys.getenv("POD_TABLE")))
    
    pod_lkup <- pod_table %>% 
      mutate(ENTITYCODE = as.integer(Entity),
             DEATHCODE = as.integer(Death_Code)) %>% 
      dplyr::select(ENTITYCODE, DEATHCODE,
                    NHS.INDICATOR = NHS_Ind) %>% 
      left_join(read.csv("data/POD lookup.csv"), by = c("DEATHCODE", "NHS.INDICATOR"),
                copy = TRUE)
    pod_lkup_2 <- pod_lkup %>%
      distinct(DEATHCODE, NHS.INDICATOR, POD2 = POD)
    
    pod_filter <- pod_lookup() %>%
      filter(pod_input %in% pod) %>%
      pull(pod_filter)
    
    lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
      # filter(Commest != "H") %>%
      mutate(Commest = ifelse(Commest == "E", 
                              "21", # ENTITYCODE 21 is classified as "other"
                              ifelse(Commest == "H", 
                                     "-799", # negative number so it doesn't join to anything
                                     Commest))) %>%
      left_join(pod_lkup, by = c("Commest" = "ENTITYCODE")) %>%
      left_join(pod_lkup_2, by = c("Est_Type" = "DEATHCODE", "NHS_Ind" = "NHS.INDICATOR")) %>%
      mutate(POD_out = ifelse(Commest == "-799", "home",
                              ifelse(is.na(POD), POD2, POD))) %>%
      filter(POD_out %in% pod_filter)
  }
  
  lkp <- tbl(con, in_schema(Sys.getenv("LOOKUPS_DATABASE"), Sys.getenv("LTLA_LKP_TABLE")))
  
  utlas <- lkp %>%
    dplyr::select(UTLA19CD) %>%
    collect() %>%
    pull()
  
  if (deprivation == FALSE) {
    lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
      left_join(lkp, by = c("LA_PC" = "LTLA13CD"))  
  } else if (deprivation == TRUE) {
    dep_lkp <- tbl(con, in_schema(Sys.getenv("LOOKUPS_DATABASE"), Sys.getenv("LSOA_DEMOGRAPHICS_TABLE")))
    
    rgns <- lkp %>%
      dplyr::select(RGN09CD) %>%
      collect() %>%
      pull()
    
    lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
      left_join(dep_lkp, by = c("LSOA11_PC" = "LSOA11CD")) %>%
      rename(Deprivation_Quintile = IMD2019_Quintiles_LSOA11_England,
             LSOA11CD = LSOA11_PC)
  }
    
  
  lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
    left_join(agegroup_lkp, by = c("xAGE_Year" = "Age"), copy = TRUE)
  
  if (deprivation == TRUE) {
    if (ethnicity == TRUE) {
      # ethnic group not in data yet because it wil be calculated once data retrieved from SQL server
      lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
        group_by(RGN09CD, Ethnic_Group, Deprivation_Quintile, Sex, Age_Group, Reg_Date)
    } else if (ethnicity == FALSE) {
      lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
        group_by(RGN09CD, Deprivation_Quintile, Sex, Age_Group, Reg_Date)
    }
  } else if (deprivation == FALSE) {
    if (ethnicity == TRUE) {
      # lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
      #   group_by(UTLAApr19CD, Ethnic_Group, Sex, Age_Group, Reg_Date) %>%
      #   summarise(deaths_total = n())
    } else if (ethnicity == FALSE) {
      lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
        rename(UTLAApr19CD = UTLA19CD) %>% 
        group_by(UTLAApr19CD, Sex, Age_Group, Reg_Date)
    }
  }
  
    
 
  
  lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
    summarise(deaths_total = n()) %>%
    collect() %>%
    ungroup() %>%
    mutate(Reg_Date = as.Date(Reg_Date, format = "%Y%m%d"),
           Sex = as.integer(Sex))
  
  if (include_2019 == TRUE) {
    deaths_2019 <- baseline_deaths_2019(ucod = ucod, btw_ucod = btw_ucod, ucods = ucods, cod = cod,
                                        pod = pod, deprivation = deprivation, ethnicity = ethnicity,
                                        age_filter = age_filter, age_group_type = age_group_type)
    
    lower_geogs_deaths_table <- bind_rows(lower_geogs_deaths_table,
                                          deaths_2019)
    
    max_date <- as.Date("2019-12-31")
  } else {
    max_date <- as.Date("2018-12-31")
  }
  
  # form complete tables with zeros for dates with no deaths
  
  
  if (deprivation == TRUE) {
    if (ethnicity == TRUE) {
      
      
    } else if (ethnicity == FALSE) {
      lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
        complete(RGN09CD = rgns,
                 Deprivation_Quintile = 1:5,
                 Reg_Date = seq.Date(from = as.Date("2015-01-01"),
                                     to = max_date,
                                     by = "days"),
                 Age_Group = unique(agegroup_lkp$Age_Group),
                 Sex,
                 fill = list(deaths_total = 0)) %>%
        arrange(RGN09CD, Deprivation_Quintile, Sex, Age_Group, Reg_Date)
      
    }
  } else if (deprivation == FALSE) {
    if (ethnicity == TRUE) {
      lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
        complete(UTLAApr19CD = utlas,
                 Ethnic_Group = ethnic_groups,
                 Reg_Date = seq.Date(from = as.Date("2015-01-01"),
                                     to = max_date,
                                     by = "days"),
                 Age_Group = unique(agegroup_lkp$Age_Group),
                 Sex,
                 fill = list(deaths_total = 0)) %>%
        arrange(UTLAApr19CD, Ethnic_Group, Sex, Age_Group, Reg_Date)
      
    } else if (ethnicity == FALSE) {
      lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
        complete(UTLAApr19CD = utlas,
                 Reg_Date = seq.Date(from = as.Date("2015-01-01"),
                                     to = max_date,
                                     by = "days"),
                 Age_Group = unique(agegroup_lkp$Age_Group),
                 Sex,
                 fill = list(deaths_total = 0)) %>%
        arrange(UTLAApr19CD, Sex, Age_Group, Reg_Date)
    }
  }
  
  dbDisconnect(con)
  return(lower_geogs_deaths_table)
  
}

#' @description function returns daily deaths by geography, age and sex for 2020. The variables
#'   in the table are in line with get_baseline_deaths where the same inputs are provided
#' @inheritParams get_baseline_deaths
#' @param covid_only logical, whether to reutrn deaths with an underlying cause or mention of
#'   COVID-19 on the death certificate
#' @param end_date date, final date for the data extract
#' @param all_ucod logical, whether to attach the underlying cause of death field to the final table
#' @param all_pod logical, whether to attach a place of death field to the final table
get_recent_deaths <- function(ucod = NULL, btw_ucod = NULL, ucods = NULL, cod = NULL,
                              pod = NULL, covid_only = FALSE,
                              deprivation = FALSE, ethnicity = FALSE, end_date, 
                              all_ucod = FALSE, all_pod = FALSE, age_filter = NULL) {
  
  if (!is.null(pod)) {
    if (length(setdiff(pod, c("home", "care home", "hospital", "hospice", "other"))) > 0) {
      stop("pod not defined properly")
    }
  }
  
  
  source("R/utils.R")
  if (ethnicity == TRUE & deprivation == TRUE) {
    agegroup_lkp <- age_group_lkp(age_filter = age_filter, type = "nomis")
  } else {
    agegroup_lkp <- age_group_lkp(age_filter = age_filter)
  }
  
  
  con <- dbConnect(odbc(), 
                   Driver = "SQL Server", 
                   Server = Sys.getenv("DATA_LAKE_SERVER"), 
                   Database = Sys.getenv("RECENT_DEATHS_DATABASE"), 
                   Trusted_Connection = "True",
                   timeout = 120)
  
  
  
  source_table <- tbl(con, in_schema(Sys.getenv("RECENT_DEATHS_DATABASE"), Sys.getenv("DEATHS_VIEW_RECENT")))
  
  recent_deaths_lower_geographies <- source_table %>%
    filter(DOR >= as.Date("2020-03-01"),
           DOR <= end_date,
           GOR9R %like% "E%",
           SEX %in% c(1, 2),
           DATAQUAL %in% c(1, 2),
           AGEC != "") 
  
  
  if (covid_only == TRUE) {
    recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
      filter(COVID19mention == 1 | COVID19UCOD == 1) 
  }
  
  if (!is.null(ucod)) {
    if (length(ucod) == 1) {
      if (grepl("%", ucod)){
        recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
          filter(ICD10U %like% ucod)
      } else {
        recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
          filter(ICD10U == ucod)
      }
    } else {
      recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
        filter(ICD10U %in% ucod)
    }
  } else if (!is.null(ucods)) {
    recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
      filter(substr(ICD10U, 1, 3) %in% ucods)  
  } else if (!is.null(btw_ucod)) {
    lower_code <- btw_ucod[1]
    upper_code <- btw_ucod[2]
    recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
      filter(ICD10U >= lower_code,
             ICD10U < upper_code)
  } else if (!is.null(cod)) {
    recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
      filter(substr(ICD10U, 1, 3) %in% cod |
               substr(ICD10_1, 1, 3) %in% cod |
               substr(ICD10_2, 1, 3) %in% cod |
               substr(ICD10_3, 1, 3) %in% cod |
               substr(ICD10_4, 1, 3) %in% cod |
               substr(ICD10_5, 1, 3) %in% cod |
               substr(ICD10_6, 1, 3) %in% cod |
               substr(ICD10_7, 1, 3) %in% cod |
               substr(ICD10_8, 1, 3) %in% cod |
               substr(ICD10_9, 1, 3) %in% cod |
               substr(ICD10_10, 1, 3) %in% cod |
               substr(ICD10_11, 1, 3) %in% cod |
               substr(ICD10_12, 1, 3) %in% cod |
               substr(ICD10_13, 1, 3) %in% cod |
               substr(ICD10_14, 1, 3) %in% cod |
               substr(ICD10_15, 1, 3) %in% cod)
  }
  
  if (all_ucod == TRUE) {
    cause_code_lkp <- cause_code_lookup(table_output = TRUE)
    recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
      mutate(ucod = substr(ICD10U, 1, 3)) %>%
      left_join(cause_code_lkp, by = c("ucod" = "ICD_codes"), copy = TRUE) %>% 
      filter(name_of_cause != "COVID-19")
  }
  
  if (!is.null(pod) | all_pod == TRUE) {
    pod_table <- tbl(con, in_schema(Sys.getenv("BIRTHS_DEATHS_DATABASE"), Sys.getenv("POD_TABLE")))
    
    pod_lkup <- pod_table %>% 
      mutate(ENTITYCODE = as.integer(Entity),
             DEATHCODE = as.integer(Death_Code)) %>% 
      dplyr::select(ENTITYCODE, DEATHCODE,
                    NHS.INDICATOR = NHS_Ind) %>% 
      left_join(read.csv("data/POD lookup.csv"), by = c("DEATHCODE", "NHS.INDICATOR"),
                copy = TRUE) %>% 
      rename(DEATHCODE_LKP = DEATHCODE) %>% 
      collect()
    pod_lkup_2 <- pod_lkup %>%
      dplyr::select(DEATHCODE_LKP_2 = DEATHCODE_LKP, NHS.INDICATOR, POD2 = POD) %>%
      unique()
    
    pod_filter <- pod_lookup() %>%
      filter(pod_input %in% pod) %>%
      pull(pod_filter)
    
    recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
      mutate(CESTRSS = ifelse(CESTRSS %in% c("E", ""), 
                              "21", # ENTITYCODE 21 is classified as "other"
                              ifelse(CESTRSS == "H", 
                                     "-799", # negative number so it doesn't join to anything
                                     CESTRSS))) %>%
      left_join(pod_lkup, by = c("CESTRSS" = "ENTITYCODE"), copy = TRUE) %>%
      left_join(pod_lkup_2, by = c("ESTTYPED" = "DEATHCODE_LKP_2", "NHSIND" = "NHS.INDICATOR"), copy = TRUE) %>%
      mutate(POD_out = ifelse(CESTRSS == "-799", "Home",
                              ifelse(is.na(POD), POD2, POD)))
      
    if (all_pod == FALSE) {
      recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
        filter(POD_out %in% pod_filter)
    }
  } 
  
  lkp <- tbl(con, in_schema(Sys.getenv("LOOKUPS_DATABASE"), Sys.getenv("LTLA19_LKP_TABLE"))) %>% 
    rename(LTLAApr19CD = LTLA19CD,
           UTLAApr19CD = UTLA19CD)
  
  utlas <- lkp %>%
    dplyr::select(UTLAApr19CD) %>%
    collect() %>%
    pull()
  
  if (deprivation == FALSE) {
    recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
      left_join(lkp, by = c("CTYDR" = "LTLAApr19CD")) %>%
      mutate(UTLAApr19CD = ifelse(is.na(UTLAApr19CD) & CTYDR == "E06000060", #New District code introduced into data - Apr 20
                                  "E10000002", UTLAApr19CD))
  } else if (deprivation == TRUE) {

    dep_lkp <- tbl(con, in_schema(Sys.getenv("LOOKUPS_DATABASE"), Sys.getenv("LSOA_DEMOGRAPHICS_TABLE"))) %>%
      dplyr::select(LSOA11CD, IMD2019_Quintiles_LSOA11_England, UTLAApr19CD = UTLA19CD)
    
    recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
      left_join(dep_lkp, by = c("LSOAR" = "LSOA11CD")) %>%
      rename(Deprivation_Quintile = IMD2019_Quintiles_LSOA11_England)
  }
  
  if (ethnicity == TRUE) {
    ethnic_group_names <- ethnic_groups_lookup() %>%
      distinct(Ethnic_Group, Ethnic_group_data_lake) %>% 
      filter(Ethnic_Group != "Unknown") # this removes a one to many join below
    
    eth_lkp <- tbl(con, in_schema(Sys.getenv("RECENT_DEATHS_DATABASE"), Sys.getenv("ETHNICITY_LINKED_DEATHS_VIEW_RECENT"))) %>%
      rename(Ethnic_group_data_lake = Ethnic_group) %>%
      left_join(ethnic_group_names, by = "Ethnic_group_data_lake", copy = TRUE) %>%
      dplyr::select(`LEDR ID` = `ledr id`, Ethnic_Group)
    
    recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
      left_join(eth_lkp, by = "LEDR ID") %>%
      mutate(Ethnic_Group = ifelse(is.na(Ethnic_Group), "Unknown", Ethnic_Group))
    
    ethnic_groups <- unique(ethnic_groups_lookup()$Ethnic_Group)
  }
  
  recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
    mutate(Age = ifelse(AGECUNIT != 1, 0, as.numeric(AGEC)))
  
  if (!is.null(age_filter)) {
    ages <- convert_age_filter(age_filter)
    lower_age <- ages$lower_age
    upper_age <- ages$upper_age
    
    recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>% 
      filter(between(Age, lower_age, upper_age))
  }
  
  recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
    left_join(agegroup_lkp, by = "Age", copy = TRUE)
  
  if (deprivation == TRUE) {
    if (ethnicity == TRUE) {
      recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
        group_by(UTLAApr19CD, Deprivation_Quintile, Ethnic_Group, SEX, Age_Group, DOR)
    } else if (ethnicity == FALSE) {
      recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
        group_by(UTLAApr19CD, Deprivation_Quintile, SEX, Age_Group, DOR)
    }
  } else if (deprivation == FALSE) {
    if (ethnicity == TRUE) {
      recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
        group_by(UTLAApr19CD, Ethnic_Group, SEX, Age_Group, DOR)
    } else if (ethnicity == FALSE) {
      if (all_ucod == TRUE) {
        recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
          group_by(UTLAApr19CD, SEX, Age_Group, DOR, name_of_cause)
      } else if (all_pod == TRUE) {
        recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
          group_by(UTLAApr19CD, SEX, Age_Group, DOR, POD_out)
      } else {
        recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
          group_by(UTLAApr19CD, SEX, Age_Group, DOR)
      }
    }
  }
 
  recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
    summarise(deaths_total = n()) %>%
    rename(Reg_Date = DOR,
           Sex = SEX) %>%
    collect() %>%
    ungroup() %>%
    mutate(Reg_Date = as.Date(Reg_Date),
           Sex = as.integer(Sex))
  
  
  # form complete tables with zeros for dates with no deaths
  
  
  if (deprivation == TRUE) {
    if (ethnicity == TRUE) {
      recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
        complete(UTLAApr19CD = utlas,
                 Ethnic_Group = ethnic_groups,
                 Deprivation_Quintile = 1:5,
                 Reg_Date = seq.Date(from = as.Date("2020-03-01"),
                                     to = max(Reg_Date, na.rm = TRUE),
                                     by = "days"),
                 Age_Group,
                 Sex,
                 fill = list(deaths_total = 0)) %>%
        arrange(UTLAApr19CD, Ethnic_Group, Deprivation_Quintile, Sex, Age_Group, Reg_Date)
    } else if (ethnicity == FALSE) {
      recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
        complete(UTLAApr19CD = utlas,
                 Deprivation_Quintile = 1:5,
                 Reg_Date = seq.Date(from = as.Date("2020-03-01"),
                                     to = max(Reg_Date, na.rm = TRUE),
                                     by = "days"),
                 Age_Group,
                 Sex,
                 fill = list(deaths_total = 0)) %>%
        arrange(UTLAApr19CD, Deprivation_Quintile, Sex, Age_Group, Reg_Date)
      
    }
  } else if (deprivation == FALSE) {
    if (ethnicity == TRUE) {
      recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
        complete(UTLAApr19CD = utlas,
                 Ethnic_Group = ethnic_groups,
                 Reg_Date = seq.Date(from = as.Date("2020-03-01"),
                                     to = max(Reg_Date, na.rm = TRUE),
                                     by = "days"),
                 Age_Group,
                 Sex,
                 fill = list(deaths_total = 0)) %>%
        arrange(UTLAApr19CD, Ethnic_Group, Sex, Age_Group, Reg_Date)
      
    } else if (ethnicity == FALSE) {
      if (all_ucod == TRUE) {
        cause_names <- cause_code_lkp %>% 
          filter(name_of_cause != "COVID-19") %>% 
          pull() %>% 
          unique()
        recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
          complete(UTLAApr19CD = utlas,
                   name_of_cause = cause_names,
                   Reg_Date = seq.Date(from = as.Date("2020-03-01"),
                                       to = max(Reg_Date, na.rm = TRUE),
                                       by = "days"),
                   Age_Group,
                   Sex,
                   fill = list(deaths_total = 0)) %>%
          arrange(UTLAApr19CD, name_of_cause, Sex, Age_Group, Reg_Date)
      } else if (all_pod == TRUE) {
        recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
          complete(UTLAApr19CD = utlas,
                   POD_out = POD_out,
                   Reg_Date = seq.Date(from = as.Date("2020-03-01"),
                                       to = max(Reg_Date, na.rm = TRUE),
                                       by = "days"),
                   Age_Group,
                   Sex,
                   fill = list(deaths_total = 0)) %>%
          arrange(UTLAApr19CD, POD_out, Sex, Age_Group, Reg_Date)
      } else {
        recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
          complete(UTLAApr19CD = utlas,
                   Reg_Date = seq.Date(from = as.Date("2020-03-01"),
                                       to = max(Reg_Date, na.rm = TRUE),
                                       by = "days"),
                   Age_Group,
                   Sex,
                   fill = list(deaths_total = 0)) %>%
          arrange(UTLAApr19CD, Sex, Age_Group, Reg_Date)
      }
      
    }
  }
  
  dbDisconnect(con)
  return(recent_deaths_lower_geographies)
}

#' @description function to apply the proportion of deaths in each ethnic group for a given day, location, 
#'   age group and sex to a separate table that contains deaths in fields that are outside the five top level
#'   ethnic groups
#' @param data table of deaths that includes fields for ethnic group, deaths, location, age group, sex and date
#' @param proportions a table containing the proportion of deaths in each ethnic group on a given day, location
#'   age group, sex. If this is NULL (default value) the proportions from the data object is applied
#' @param ethnicity_field unquoted name of field that contains ethnic groups
#' @param deaths_field unquoted name of field that contains the death count
#' @param region_field unquoted name of field that contains the location information
#' @param age_field unquoted name of field that contains age groups
#' @param sex_field unquoted name of field that contains sex
#' @param date_field unquoted name of field that contains date
ethnicity_not_stated_adjustment <- function(data, proportions = NULL, ethnicity_field, 
                                            deaths_field, region_field, age_field, sex_field, date_field,
                                            include_deprivation = FALSE) {
  ethnicity_groups <- c("Asian", "Black", "Mixed", "Other", "White")
  
  copy_proportions <- !is.null(proportions)
  if (is.null(proportions)) proportions <- calculate_ethnicity_proportions(data, {{ ethnicity_field }}, 
                                                                           {{ region_field }}, {{ age_field }}, 
                                                                           {{ sex_field }}, {{ deaths_field }},
                                                                           include_deprivation = include_deprivation)
  
  if (include_deprivation == FALSE) {
    unknown_deaths <- data %>%
      filter(!({{ ethnicity_field }} %in% ethnicity_groups)) %>%
      group_by({{ region_field }}, {{ age_field }}, {{ sex_field }}, {{ date_field }}) %>%
      summarise(summed_deaths = sum({{ deaths_field }}), 
                .groups = "drop") %>%
      left_join(proportions, by = intersect(names(.), names(proportions)), copy = copy_proportions) %>%
      mutate(unknown_deaths = summed_deaths * proportion) %>%
      dplyr::select({{ region_field }}, {{ age_field }}, {{ sex_field }}, {{ ethnicity_field }}, {{ date_field }}, unknown_deaths)
    
    recalculated_data <- data %>%
      filter({{ ethnicity_field }} %in% ethnicity_groups) %>%
      left_join(unknown_deaths, by = intersect(names(.), names(unknown_deaths))) %>%
      mutate({{ deaths_field }} := {{ deaths_field }} + unknown_deaths) %>%
      dplyr::select(-unknown_deaths)
  } else {
    unknown_deaths <- data %>%
      filter(!({{ ethnicity_field }} %in% ethnicity_groups)) %>%
      group_by({{ region_field }}, {{ age_field }}, {{ sex_field }}, {{ date_field }}) %>%
      summarise(summed_deaths = sum({{ deaths_field }}), 
                .groups = "drop") %>%
      left_join(proportions, by = intersect(names(.), names(proportions)), copy = copy_proportions) %>%
      mutate(unknown_deaths = summed_deaths * proportion) %>%
      dplyr::select({{ region_field }}, {{ age_field }}, {{ sex_field }}, {{ ethnicity_field }}, Deprivation_Quintile, {{ date_field }}, unknown_deaths)
    
    recalculated_data <- data %>%
      filter({{ ethnicity_field }} %in% ethnicity_groups) %>%
      left_join(unknown_deaths, by = intersect(names(.), names(unknown_deaths))) %>%
      mutate({{ deaths_field }} := {{ deaths_field }} + unknown_deaths) %>%
      dplyr::select(-unknown_deaths)
  }
  
  
  return(recalculated_data)
}

#' @description function to calculate the proportion of deaths in each ethnic group for a given day, location, 
#'   age group and sex
#' @inheritParams ethnicity_not_stated_adjustment
calculate_ethnicity_proportions <- function(data, ethnicity_field, region_field, age_field, sex_field, deaths_field, include_deprivation = FALSE) {
  
  ethnicity_groups <- c("Asian", "Black", "Mixed", "Other", "White")
  
  if (include_deprivation == FALSE) {
    proportions <- data %>%
      filter({{ ethnicity_field }} %in% ethnicity_groups) %>%
      group_by({{ region_field }}, {{ age_field }}, {{ sex_field }}, {{ ethnicity_field }}) %>%
      summarise(summed_deaths := sum({{ deaths_field }}), .groups = "drop_last") %>%
      mutate(proportion = summed_deaths / sum(summed_deaths)) %>%
      ungroup() %>%
      dplyr::select(-summed_deaths)
  } else {
    proportions <- data %>%
      filter({{ ethnicity_field }} %in% ethnicity_groups) %>%
      group_by({{ region_field }}, {{ age_field }}, {{ sex_field }}, {{ ethnicity_field }}, Deprivation_Quintile) %>%
      summarise(summed_deaths := sum({{ deaths_field }}), .groups = "keep") %>%
      group_by({{ region_field }}, {{ age_field }}, {{ sex_field }}) %>% 
      mutate(proportion = summed_deaths / sum(summed_deaths)) %>%
      ungroup() %>%
      dplyr::select(-summed_deaths)
  }
  
  
  return(proportions)
}

#' @description function returning death counts for MBIS data in 2019 by date,
#'   sex, age group, ethnicity and deprivation following the methodology of
#'   baseline
#' @inheritParams get_baseline_deaths
baseline_deaths_2019 <- function(ucod = NULL, btw_ucod = NULL, ucods = NULL, cod = NULL,
                                 pod = NULL, deprivation = FALSE, ethnicity = FALSE, age_filter = NULL,
                                 age_group_type = "original") {
  
  con <- dbConnect(odbc(), 
                   Driver = "SQL Server", 
                   Server = Sys.getenv("DATA_LAKE_SERVER"), 
                   Database = Sys.getenv("MONTHLY_DEATHS_DATABASE"), 
                   Trusted_Connection = "True",
                   timeout = 120)
  
  if (age_group_type == "original") {
    agegroup_lkp <- age_group_lkp(age_filter = age_filter, type = age_group_type) %>%
      distinct(Age_Group, Age_Group_PHA)
    
    duplicates_2019 <- tbl(con, in_schema(Sys.getenv("MONTHLY_DEATHS_DATABASE"), Sys.getenv("DEATHS_VIEW_MONTHLY_DUPLICATES"))) %>%
      rename(Reg_Date = DOR, 
             Sex = SEX_STATISTICAL,
             UTLAApr19CD = UTLA_code,
             Age_Group = Age_group)
    
    deaths_2019 <- tbl(con, in_schema(Sys.getenv("MONTHLY_DEATHS_DATABASE"), Sys.getenv("DEATHS_VIEW_MONTHLY_DISTINCT"))) %>%
      rename(Reg_Date = DOR, 
             Sex = SEX_STATISTICAL,
             UTLAApr19CD = UTLA_code,
             Age_Group = Age_group)
  
  } else {
    agegroup_lkp <- age_group_lkp(age_filter = age_filter, type = age_group_type) %>% 
      distinct(Age, Age_Group)
    
    duplicates_2019 <- tbl(con, in_schema(Sys.getenv("MONTHLY_DEATHS_DATABASE"), Sys.getenv("DEATHS_VIEW_MONTHLY_DUPLICATES"))) %>%
      rename(PHA_age_group = Age_group) %>% 
      left_join(agegroup_lkp, by = c("xAGE_Year" = "Age"), copy = TRUE) %>% 
      rename(Reg_Date = DOR, 
             Sex = SEX_STATISTICAL,
             UTLAApr19CD = UTLA_code)
    
    deaths_2019 <- tbl(con, in_schema(Sys.getenv("MONTHLY_DEATHS_DATABASE"), Sys.getenv("DEATHS_VIEW_MONTHLY_DISTINCT"))) %>%
      rename(PHA_age_group = Age_group) %>% 
      left_join(agegroup_lkp, by = c("xAGE_Year" = "Age"), copy = TRUE) %>% 
      rename(Reg_Date = DOR, 
             Sex = SEX_STATISTICAL,
             UTLAApr19CD = UTLA_code)
  }
  
  if (!is.null(age_filter)) {
    ages <- convert_age_filter(age_filter)
    lower_age <- ages$lower_age
    upper_age <- ages$upper_age
    
    duplicates_2019 <- duplicates_2019 %>% 
      mutate(xAGE_Year = as.integer(xAGE_Year)) %>% 
      filter(between(xAGE_Year, lower_age, upper_age))
    
    deaths_2019 <- deaths_2019 %>% 
      mutate(xAGE_Year = as.integer(xAGE_Year)) %>% 
      filter(between(xAGE_Year, lower_age, upper_age))
  }
  
  # cause of death filter
  if (!is.null(ucod)) {
    if (length(ucod) == 1) {
      if (grepl("%", ucod)){
        duplicates_2019 <- duplicates_2019 %>%
          filter(ICDU %like% ucod)
        
        deaths_2019 <- deaths_2019 %>%
          filter(ICDU %like% ucod)
      } else {
        duplicates_2019 <- duplicates_2019 %>%
          filter(ICDU == ucod)
        
        deaths_2019 <- deaths_2019 %>%
          filter(ICDU == ucod)
      }
    } else {
      duplicates_2019 <- duplicates_2019 %>%
        filter(ICDU %in% ucod)
      
      deaths_2019 <- deaths_2019 %>%
        filter(ICDU %in% ucod)
    }
  } else if (!is.null(ucods)) {
    duplicates_2019 <- duplicates_2019 %>%
      filter(substr(ICDU, 1, 3) %in% ucods) 
    
    deaths_2019 <- deaths_2019 %>%
      filter(substr(ICDU, 1, 3) %in% ucods) 
  } else if (!is.null(btw_ucod)) {
    lower_code <- btw_ucod[1]
    upper_code <- btw_ucod[2]
    
    duplicates_2019 <- duplicates_2019 %>%
      filter(ICDU >= lower_code,
             ICDU < upper_code)
    
    deaths_2019 <- deaths_2019 %>%
      filter(ICDU >= lower_code,
             ICDU < upper_code)
  } else if (!is.null(cod)) {
    duplicates_2019 <- duplicates_2019 %>%
      filter(substr(ICDU, 1, 3) %in% cod |
               substr(ICDUF, 1, 3) %in% cod |
               substr(ICD_1, 1, 3) %in% cod |
               substr(ICD_2, 1, 3) %in% cod |
               substr(ICD_3, 1, 3) %in% cod |
               substr(ICD_4, 1, 3) %in% cod |
               substr(ICD_5, 1, 3) %in% cod |
               substr(ICD_6, 1, 3) %in% cod |
               substr(ICD_7, 1, 3) %in% cod |
               substr(ICD_8, 1, 3) %in% cod |
               substr(ICD_9, 1, 3) %in% cod |
               substr(ICD_10, 1, 3) %in% cod |
               substr(ICD_11, 1, 3) %in% cod |
               substr(ICD_12, 1, 3) %in% cod |
               substr(ICD_13, 1, 3) %in% cod |
               substr(ICD_14, 1, 3) %in% cod |
               substr(ICD_15, 1, 3) %in% cod)
    
    deaths_2019 <- deaths_2019 %>%
      filter(substr(ICDU, 1, 3) %in% cod |
               substr(ICDUF, 1, 3) %in% cod |
               substr(ICD_1, 1, 3) %in% cod |
               substr(ICD_2, 1, 3) %in% cod |
               substr(ICD_3, 1, 3) %in% cod |
               substr(ICD_4, 1, 3) %in% cod |
               substr(ICD_5, 1, 3) %in% cod |
               substr(ICD_6, 1, 3) %in% cod |
               substr(ICD_7, 1, 3) %in% cod |
               substr(ICD_8, 1, 3) %in% cod |
               substr(ICD_9, 1, 3) %in% cod |
               substr(ICD_10, 1, 3) %in% cod |
               substr(ICD_11, 1, 3) %in% cod |
               substr(ICD_12, 1, 3) %in% cod |
               substr(ICD_13, 1, 3) %in% cod |
               substr(ICD_14, 1, 3) %in% cod |
               substr(ICD_15, 1, 3) %in% cod)
  }
  
  # place of death filter
  if (!is.null(pod)) {
    pod_table <- tbl(con, in_schema(Sys.getenv("BIRTHS_DEATHS_DATABASE"), Sys.getenv("POD_TABLE")))
    
    pod_lkup <- pod_table %>% 
      mutate(ENTITYCODE = as.integer(Entity),
             DEATHCODE = as.integer(Death_Code)) %>% 
      dplyr::select(ENTITYCODE, DEATHCODE,
                    NHS.INDICATOR = NHS_Ind) %>% 
      left_join(read.csv("data/POD lookup.csv"), by = c("DEATHCODE", "NHS.INDICATOR"),
                copy = TRUE) %>% 
      collect()
    pod_lkup_2 <- pod_lkup %>%
      dplyr::select(DEATHCODE, NHS.INDICATOR, POD2 = POD) %>%
      unique()
    
    pod_filter <- pod_lookup() %>%
      filter(pod_input %in% pod) %>%
      pull(pod_filter)
    
    duplicates_mbisids_2019 <- duplicates_2019 %>% 
      mutate(CESTRSS = ifelse(CESTRSS == "E", 
                              "21", # ENTITYCODE 21 is classified as "other"
                              ifelse(CESTRSS == "H", 
                                     "-799", # negative number so it doesn't join to anything
                                     CESTRSS))) %>%
      left_join(pod_lkup, by = c("CESTRSS" = "ENTITYCODE"), copy = TRUE) %>%
      left_join(pod_lkup_2, by = c("ESTTYPED" = "DEATHCODE", "NHSIND" = "NHS.INDICATOR"), copy = TRUE) %>%
      mutate(POD_out = ifelse(CESTRSS == "-799", "home",
                              ifelse(is.na(POD), POD2, POD))) %>%
      filter(POD_out %in% pod_filter) %>%
      distinct(MBISID, Reg_Date) %>%
      group_by(MBISID) %>%
      filter(n() == 1) %>%
      ungroup() %>%
      dplyr::select(MBISID) %>%
      collect() %>%
      pull(MBISID)
    
    duplicates_2019 <- duplicates_2019 %>%
      filter(MBISID %in% duplicates_mbisids_2019)
    
    deaths_2019 <- deaths_2019 %>%
      mutate(CESTRSS = ifelse(CESTRSS == "E", 
                              "21", # ENTITYCODE 21 is classified as "other"
                              ifelse(CESTRSS == "H", 
                                     "-799", # negative number so it doesn't join to anything
                                     CESTRSS))) %>%
      left_join(pod_lkup, by = c("CESTRSS" = "ENTITYCODE"), copy = TRUE) %>%
      left_join(pod_lkup_2, by = c("ESTTYPED" = "DEATHCODE", "NHSIND" = "NHS.INDICATOR"), copy = TRUE) %>%
      mutate(POD_out = ifelse(CESTRSS == "-799", "home",
                              ifelse(is.na(POD), POD2, POD))) %>%
      filter(POD_out %in% pod_filter)
  }
  
  lkp <- tbl(con, in_schema(Sys.getenv("LOOKUPS_DATABASE"), Sys.getenv("UTLA19_LKP_TABLE"))) %>% 
    rename(UTLAApr19CD = UTLA19CD)
  
  utlas <- lkp %>%
    dplyr::select(UTLAApr19CD) %>%
    collect() %>%
    pull()
  
  if (deprivation == FALSE) {
    deaths_2019 <- deaths_2019 %>%
      left_join(lkp, by = "UTLAApr19CD")
    
    duplicates_2019 <- duplicates_2019 %>%
      left_join(lkp, by = "UTLAApr19CD")
  } else if (deprivation == TRUE) {
    dep_lkp <- tbl(con, in_schema(Sys.getenv("LOOKUPS_DATABASE"), Sys.getenv("LSOA_DEMOGRAPHICS_TABLE")))
    
    rgns <- lkp %>%
      dplyr::select(RGN09CD) %>%
      collect() %>%
      pull()
    
    deaths_2019 <- deaths_2019 %>%
      left_join(dep_lkp, by = c("LSOAR" = "LSOA11CD")) %>%
      rename(Deprivation_Quintile = IMD2019_Quintiles_LSOA11_England,
             LSOA11CD = LSOAR)
    
    duplicates_2019 <- duplicates_2019 %>%
      left_join(dep_lkp, by = c("LSOAR" = "LSOA11CD")) %>%
      rename(Deprivation_Quintile = IMD2019_Quintiles_LSOA11_England,
             LSOA11CD = LSOAR)
    
  }
  
  # get distinct records from duplicates table
  # combine with "distinct" table perform count
  # perform count
  
  if (deprivation == TRUE) {
    if (ethnicity == TRUE) {
      stop("Ethnicity not available for 2019")
      
    } else if (ethnicity == FALSE) {
      duplicates_2019 <- duplicates_2019 %>%
        dplyr::select(MBISID, RGN09CD, Deprivation_Quintile, Sex, Age_Group, Reg_Date) %>%
        distinct(MBISID, RGN09CD, Deprivation_Quintile, Sex, Age_Group, Reg_Date) %>%
        group_by(MBISID) %>%
        filter(n() == 1) %>%
        ungroup() %>%
        group_by(RGN09CD, Deprivation_Quintile, Sex, Age_Group, Reg_Date) %>%
        summarise(deaths_total = n())
      
      deaths_2019 <- deaths_2019 %>%
        dplyr::select(MBISID, RGN09CD, Deprivation_Quintile, Sex, Age_Group, Reg_Date) %>%
        group_by(RGN09CD, Deprivation_Quintile, Sex, Age_Group, Reg_Date) %>%
        summarise(deaths_total = n())
      
    }
  } else if (deprivation == FALSE) {
    if (ethnicity == TRUE) {
      stop("Ethnicity not available for 2019")
      
    } else if (ethnicity == FALSE) {
      duplicates_2019 <- duplicates_2019 %>%
        dplyr::select(MBISID, UTLAApr19CD, Sex, Age_Group, Reg_Date) %>%
        distinct(MBISID, UTLAApr19CD, Sex, Age_Group, Reg_Date) %>%
        group_by(MBISID) %>%
        filter(n() == 1) %>%
        ungroup() %>%
        group_by(UTLAApr19CD, Sex, Age_Group, Reg_Date) %>%
        summarise(deaths_total = n())
      
      deaths_2019 <- deaths_2019 %>%
        dplyr::select(MBISID, UTLAApr19CD, Sex, Age_Group, Reg_Date) %>%
        group_by(UTLAApr19CD, Sex, Age_Group, Reg_Date) %>%
        summarise(deaths_total = n())
      
    }
  }
  
  
  exclude_var <- "deaths_total"
  
  duplicates_2019 <- duplicates_2019 %>%
    collect() %>%
    ungroup()
  
  deaths_2019 <- deaths_2019 %>%
    collect() %>%
    bind_rows(duplicates_2019) %>%
    group_by_at(vars(-all_of(exclude_var))) %>%
    summarise(deaths_total = sum(deaths_total), .groups = "drop") %>%
    mutate(Reg_Date = as.Date(Reg_Date, format = "%Y%m%d"),
           Sex = as.integer(Sex))
  
  if (age_group_type == "original") {
    deaths_2019 <- deaths_2019 %>%
      rename(Age_Group_PHA = Age_Group) %>%
      left_join(agegroup_lkp, by = "Age_Group_PHA") %>%
      dplyr::select(-Age_Group_PHA)
  }
    
  
  return(deaths_2019)
}

