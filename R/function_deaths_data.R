#' @description function returns daily deaths by geography, age group and sex
#'   between 2015 and 2019
#' @details this function requires access to the BirthsDeaths database on the
#'   Data Lake. When the arguments for eth_dep is TRUE, the geography will be
#'   region. Otherwise, the region will be UTLA (April 2019)
#' @param ucod string, ICD10 codes to filter deaths in the underlying cause of
#'   death field. Allowable strings are a single value, a character vector, or a
#'   single value with a '%' at the end, which is used in the same way as a '%'
#'   in SQL (ie, LIKE 'E%)
#' @param ucods string, ICD10 codes to filter deaths in the underlying cause of
#'   death field. Vector can be any length and is used to filter the UCOD field
#'   with an %in% function (on the first 3 characters of the UCODs field)
#' @param cod string, ICD10 codes used to filter the UCOD and the 15 COD fields
#'   to identify a mention of a cause. It filters these fields with an %in%
#'   function (on the first 3 characters of the UCODs field)
#' @param pod string, vector of length up to five describing the place of death.
#'   Each item must be one of the following; "home", "care home", "hospital",
#'   "hospice", "other". Default is NULL, which represents all places
#' @param eth_dep logical, include ethnic group and deprivation quintile in all
#'   processes (data extraction, visualisation, modelling). All place of death
#'   and cause of death filters will work when this argument is TRUE
#' @param include_deprivation logical, should the output contain a field for
#'   Deprivation_Quintile. This should only be used when uplifting the ethnic
#'   group deaths from the HES-linked mortality database
#' @param include_deprivation logical, should the output contain a field for
#'   Deprivation_Quintile. This should only be used when uplifting the ethnic
#'   group deaths from the HES-linked mortality database
#' @param age_filter numeric length 2; lower and upper limits (inclusive) for
#'   age filter. Where limit is the extent of the range use NA, eg, c(75, NA) is
#'   equivalent to "75+"
#' @param age_group_type string; "original", "nomis", "bespoke" are accepted
#'   inputs. If "bespoke" is used, then the bespoke_age_groups parameter needs
#'   to be provided. "bespoke" option is currently incompatible with eth_dep =
#'   TRUE because baseline data comes from pre-calculated file
#' @param bespoke_age_groups tibble; two fields - Age and Age_Group. Age
#'   contains integers and Age_Group is a character class displaying the age
#'   group that that age is assigned to
#'
#'   
get_baseline_deaths <- function(ucod = NULL, ucods = NULL, cod = NULL,
                                pod = NULL, eth_dep = FALSE, include_deprivation = FALSE,
                                age_filter = NULL, age_group_type = "original",
                                bespoke_age_groups = NULL) {

  # check pod
  if (!is.null(pod)) {
    if (length(setdiff(pod, c("home", "care home", "hospital", "hospice", "other"))) > 0) {
      stop("pod not defined properly")
    }
  }
  
  if (age_group_type == "bespoke") {
    agegroup_lkp <- age_group_lkp(age_filter = age_filter, type = "bespoke",
                                  bespoke_age_groups = bespoke_age_groups)  
  } else if (eth_dep == TRUE |
      age_group_type == "nomis") {
    agegroup_lkp <- age_group_lkp(age_filter = age_filter, type = "nomis")  
  } else {
    agegroup_lkp <- age_group_lkp(age_filter = age_filter, type = "original")
  }
  
  # create date range
  min_date <- as.Date("2015-01-09")
  max_date <- as.Date("2019-12-27")
  
  # deprivation and ethnicity -----------------------------------------------
  if (eth_dep == TRUE) {
    utla_lkp <- utla_lookup()
    
    rgn_lkp <- utla_lkp %>% 
      distinct(RGN09NM, RGN09CD)
    
    ethnic_groups <- ethnic_groups_lookup() %>%
      distinct(Ethnic_Group, Ethnicity_Broad)
    
    filepath <- Sys.getenv("ETHNICITY_DEPRIVATION_BASELINE_DEATHS")
    lower_geogs_deaths_table <- read_excel(filepath,
                                           sheet = "EthDep",
                                           range = "R1C1:R665878C7")
    
    lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
      mutate(Reg_Date = as.Date(Reg_Date),
             Sex = as.integer(Sex)) %>%
      left_join(ethnic_groups, by = "Ethnicity_Broad") %>%
      filter(Sex %in% c(1, 2),
             between(Reg_Date, min_date - 6, max_date),
             Deprivation_Quintile != "NULL") %>%
      mutate(Deprivation_Quintile = as.integer(Deprivation_Quintile),
             Ethnic_Group = if_else(is.na(Ethnic_Group), 
                                    "Not_Stated/Not_Known/Not_Given",
                                    Ethnic_Group),
             Reg_Date = case_when(
               wday(Reg_Date) <= 6 ~ Reg_Date + (6 - wday(Reg_Date)),
               TRUE ~ Reg_Date + 6
             )) %>% 
      group_by(Region, Ethnic_Group, Deprivation_Quintile,
               Reg_Date, Age_group, Sex) %>% 
      summarise(Deaths_total = sum(Deaths_total),
                .groups = "drop") %>% 
      rename(RGN09CD = Region,
             Age_Group = Age_group, 
             deaths_total = Deaths_total)
    
    
    lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
      complete(RGN09CD = unique(rgn_lkp$RGN09CD),
               Ethnic_Group = ethnic_groups$Ethnic_Group,
               Deprivation_Quintile = 1:5,
               Reg_Date = seq.Date(from = min_date,
                                   to = max_date,
                                   by = "week"),
               Age_Group = unique(agegroup_lkp$Age_Group),
               Sex,
               fill = list(deaths_total = 0)) %>%
      arrange(RGN09CD, Ethnic_Group, Deprivation_Quintile, Sex, Age_Group, Reg_Date)
    

# this section uplifts the total deaths from the HES linked mortality database
# so they match with the total deaths from the Deaths database by applying a
# daily proportion from the HES linked mortality database to the latter database

    weekly_death_totals <- get_baseline_deaths(include_deprivation = TRUE,
                                              age_group_type = "nomis") %>% 
      # left_join(utla_lkp, by = "UTLAApr19CD") %>% 
      group_by(RGN09CD, Reg_Date, Age_Group, Sex, Deprivation_Quintile) %>% 
      summarise(deaths_total = sum(deaths_total), 
                .groups = "drop")
    
    mean_window <- 30
    
    weekly_proportions <- lower_geogs_deaths_table %>% 
      group_by(RGN09CD, Age_Group, Sex, Ethnic_Group, Deprivation_Quintile) %>% 
      mutate(average_deaths_over_period = slide_index_dbl(.x = deaths_total,
                                                          .i = Reg_Date, 
                                                          .f = mean, 
                                                          .before = mean_window, 
                                                          .after = mean_window,
                                                          .complete = FALSE)) %>% 
      group_by(Reg_Date, RGN09CD, Age_Group, Sex, Deprivation_Quintile) %>% 
      mutate(proportion = average_deaths_over_period / sum(average_deaths_over_period)) %>% 
      ungroup() %>% 
      dplyr::select(-c(deaths_total, average_deaths_over_period))
    
    
    baseline_proportions <- lower_geogs_deaths_table %>% 
      group_by(RGN09CD, Age_Group, Sex, Deprivation_Quintile, Ethnic_Group) %>% 
      summarise(deaths_over_baseline = sum(deaths_total),
                .groups = "drop_last") %>% 
      mutate(baseline_proportion = deaths_over_baseline / sum(deaths_over_baseline)) %>% 
      ungroup() %>% 
      dplyr::select(-deaths_over_baseline)
    
    weekly_proportions <- weekly_proportions %>% 
      left_join(baseline_proportions, by = c("RGN09CD", "Age_Group", "Sex", "Deprivation_Quintile", "Ethnic_Group")) %>% 
      mutate(proportion = case_when(
        is.na(proportion) ~ baseline_proportion,
        TRUE ~ proportion)) %>% 
      dplyr::select(-baseline_proportion)
    
    lower_geogs_deaths_table <- weekly_death_totals %>%
      left_join(weekly_proportions, by = c("RGN09CD", "Reg_Date", "Age_Group", "Sex", "Deprivation_Quintile")) %>%
      mutate(deaths_total = deaths_total * proportion) %>% 
      dplyr::select(-proportion) %>%
      complete(RGN09CD = unique(rgn_lkp$RGN09CD),
               Ethnic_Group = ethnic_groups$Ethnic_Group,
               Deprivation_Quintile = 1:5,
               Reg_Date = seq.Date(from = min_date,
                                   to = max_date,
                                   by = "week"),
               Age_Group = unique(agegroup_lkp$Age_Group),
               Sex,
               fill = list(deaths_total = 0)) %>%
      arrange(RGN09CD, Ethnic_Group, Deprivation_Quintile, Sex, Age_Group, Reg_Date)    
      

# End of uplift section          
      
    ethnicity_deprivation_baseline <- lower_geogs_deaths_table %>% 
      filter(Ethnic_Group %in% c("Asian", "Black", "Mixed", "Other", "White")) %>% 
      group_by(RGN09CD, Sex, Age_Group, Ethnic_Group, Deprivation_Quintile) %>% 
      summarise(deaths_total = sum(deaths_total),
                .groups = "drop")
    
    ethnicity_proportions <- calculate_ethnicity_proportions(data = ethnicity_deprivation_baseline,
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
    return(lower_geogs_deaths_table)
  }

  con <- dbConnect(odbc(), 
                   Driver = "SQL Server", 
                   Server = Sys.getenv("DATA_LAKE_SERVER"), 
                   Database = Sys.getenv("BIRTHS_DEATHS_DATABASE"), 
                   Trusted_Connection = "True",
                   timeout = 120)
  
  source_table <- tbl(con, 
                      in_schema(
                        sql(Sys.getenv("BIRTHS_DEATHS_DATABASE")), 
                        sql(Sys.getenv("DEATHS_VIEW_BEFORE_2019")))
  )
  
  first_date <- min_date - 6
  lower_geogs_deaths_table <- source_table %>%
    filter(#xYear >= 2015 & xYear <= 2019,
      between(as.Date(Reg_Date), first_date, max_date),
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
    
    nchar <- icd_codes_length(ucods)
    
    lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
      filter(substr(UCOD, 1, nchar) %in% ucods)
  } else if (!is.null(cod)) {
    
    nchar <- icd_codes_length(cod)
    
    lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
      filter(substr(UCOD, 1, nchar) %in% cod |
               substr(COD_1, 1, nchar) %in% cod |
               substr(COD_2, 1, nchar) %in% cod |
               substr(COD_3, 1, nchar) %in% cod |
               substr(COD_4, 1, nchar) %in% cod |
               substr(COD_5, 1, nchar) %in% cod |
               substr(COD_6, 1, nchar) %in% cod |
               substr(COD_7, 1, nchar) %in% cod |
               substr(COD_8, 1, nchar) %in% cod |
               substr(COD_9, 1, nchar) %in% cod |
               substr(COD_10, 1, nchar) %in% cod |
               substr(COD_11, 1, nchar) %in% cod |
               substr(COD_12, 1, nchar) %in% cod |
               substr(COD_13, 1, nchar) %in% cod |
               substr(COD_14, 1, nchar) %in% cod |
               substr(COD_15, 1, nchar) %in% cod)
  }
  
  if (!is.null(pod)) {
    pod_table <- tbl(con, 
                     in_schema(
                       sql(Sys.getenv("BIRTHS_DEATHS_DATABASE")), 
                       sql(Sys.getenv("POD_TABLE")))
    )
    
    pod_lkup <- pod_table %>% 
      mutate(ENTITYCODE = as.integer(Entity),
             DEATHCODE = as.integer(Death_Code)) %>% 
      dplyr::select(ENTITYCODE, DEATHCODE,
                    NHS.INDICATOR = NHS_Ind) %>% 
      left_join(read.csv("data/POD lookup.csv"), by = c("DEATHCODE", "NHS.INDICATOR"),
                copy = TRUE,
                auto_index = TRUE)
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
  
  lkp <- tbl(con, 
             in_schema(
               sql(Sys.getenv("LOOKUPS_DATABASE")), 
               sql(Sys.getenv("LTLA19_LKP_TABLE")))
  )
  
  
  utlas <- lkp %>%
    dplyr::select(UTLA19CD) %>%
    collect() %>%
    pull()
  
  if (include_deprivation == FALSE) {
    lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
      left_join(lkp, by = c("xONS_LTLA19" = "LTLA19CD"))  
  } else if (include_deprivation == TRUE) {
    dep_lkp <- tbl(con, 
                   in_schema(
                     sql(Sys.getenv("LOOKUPS_DATABASE")), 
                     sql(Sys.getenv("LSOA_DEMOGRAPHICS_TABLE")))
    )
    
    rgns <- lkp %>%
      dplyr::select(RGN09CD) %>%
      collect() %>%
      pull()
    
    lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
      filter(!is.na(LSOA11_PC)) %>% 
      left_join(dep_lkp, by = c("LSOA11_PC" = "LSOA11CD")) %>%
      rename(Deprivation_Quintile = IMD2019_Quintiles_LSOA11_England,
             LSOA11CD = LSOA11_PC)
  }
    
  
  lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
    left_join(agegroup_lkp, by = c("xAGE_Year" = "Age"), 
              copy = TRUE,
              auto_index = TRUE) %>% 
    mutate(# DATEPART = 6 is a Friday; reassign all registration dates to following Friday
      Reg_Date = as.Date(Reg_Date),
      Reg_Date = ifelse(
        DATEPART(sql("weekday"), Reg_Date) <= 6,
        DATEADD(sql("day"), (6 - DATEPART(sql("weekday"), Reg_Date)), Reg_Date),
        DATEADD(sql("day"), 6, Reg_Date)
      ))
            
    
  
  if (include_deprivation == TRUE) {
      lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
        group_by(RGN09CD, Deprivation_Quintile, Sex, Age_Group, Reg_Date)
    
  } else {
    lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
        rename(UTLAApr19CD = UTLA19CD) %>% 
        group_by(UTLAApr19CD, Sex, Age_Group, Reg_Date)
  }
  
  lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
    summarise(deaths_total = n(),
              .groups = "drop") %>%
    collect() %>%
    ungroup() %>%
    mutate(Sex = as.integer(Sex),
           Reg_Date = as.Date(Reg_Date))
  
  
  
  if (include_deprivation == TRUE) {
    lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
      complete(RGN09CD = rgns,
               Reg_Date = seq.Date(from = min_date,
                                   to = max_date,
                                   by = "week"),
               Age_Group = unique(agegroup_lkp$Age_Group),
               Sex = 1:2,
               Deprivation_Quintile,
               fill = list(deaths_total = 0)) %>%
      arrange(RGN09CD, Sex, Age_Group, Deprivation_Quintile, Reg_Date)
  } else {
    lower_geogs_deaths_table <- lower_geogs_deaths_table %>%
      complete(UTLAApr19CD = utlas,
               Reg_Date = seq.Date(from = min_date,
                                   to = max_date,
                                   by = "week"),
               Age_Group = unique(agegroup_lkp$Age_Group),
               Sex = 1:2,
               fill = list(deaths_total = 0)) %>%
      arrange(UTLAApr19CD, Sex, Age_Group, Reg_Date)
  }

  dbDisconnect(con)
  return(lower_geogs_deaths_table)
  
}

#' @description function returns daily deaths by geography, age and sex for
#'   2020. The variables in the table are in line with get_baseline_deaths where
#'   the same inputs are provided
#' @inheritParams get_baseline_deaths
#' @param covid_only logical, whether to return deaths with an underlying cause
#'   or mention of COVID-19 on the death certificate
#' @param end_date date, final date for the data extract
#' @param all_pod logical, whether to attach a place of death field to the final
#'   table
get_recent_deaths <- function(ucod = NULL, ucods = NULL, cod = NULL,
                              pod = NULL, covid_only = FALSE,
                              eth_dep = FALSE, end_date, 
                              all_pod = FALSE, age_filter = NULL,
                              age_group_type = "original",
                              bespoke_age_groups = NULL) {
  
  if (!is.null(pod)) {
    if (length(setdiff(pod, c("home", "care home", "hospital", "hospice", "other"))) > 0) {
      stop("pod not defined properly")
    }
  }
  
  breakdown_by_ucod <- FALSE
  if (!is.null(cod) & 
      any(!is.null(ucod), !is.null(ucods))) breakdown_by_ucod <- TRUE
  
  if (age_group_type == "bespoke") {
    agegroup_lkp <- age_group_lkp(age_filter = age_filter, type = "bespoke",
                                  bespoke_age_groups = bespoke_age_groups)  
  } else if (eth_dep == TRUE |
             age_group_type == "nomis") {
    agegroup_lkp <- age_group_lkp(age_filter = age_filter, type = "nomis")  
  } else {
    agegroup_lkp <- age_group_lkp(age_filter = age_filter, type = "original")
  }
  
  
  con <- dbConnect(odbc(), 
                   Driver = "SQL Server", 
                   Server = Sys.getenv("DATA_LAKE_SERVER"), 
                   Database = Sys.getenv("RECENT_DEATHS_DATABASE"), 
                   Trusted_Connection = "True",
                   timeout = 120)
  
  
  
  source_table <- tbl(con, 
                      in_schema(
                        sql(Sys.getenv("RECENT_DEATHS_DATABASE")), 
                        sql(Sys.getenv("DEATHS_VIEW_RECENT")))
  )
  
  first_date <- as.Date("2020-03-21")
  min_date <- first_date + 6
  
  if (wday(end_date) != 6) stop("end_date must be a Friday")
  
  recent_deaths_lower_geographies <- source_table %>%
    filter(DOR >= first_date,
           DOR <= end_date,
           GOR9R %like% "E%",
           SEX %in% c("1", "2"),
           DATAQUAL %in% c("1", "2"),
           AGEC != "") 
  
  
  if (covid_only == TRUE) {
    if (breakdown_by_ucod == FALSE) {
      recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
        filter(COVID19mention == 1 | COVID19UCOD == 1)   
    } else {
      recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
        filter(COVID19UCOD == 1) 
    }
    
  }
  
  if (breakdown_by_ucod == FALSE) {
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
      
      nchar <- icd_codes_length(ucods)
      
      recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
        filter(substr(ICD10U, 1, nchar) %in% ucods)  
    } 
  }
  
  if (!is.null(cod)) {
    
    nchar <- icd_codes_length(cod)
    
    recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
      filter(substr(ICD10U, 1, nchar) %in% cod |
               substr(ICD10_1, 1, nchar) %in% cod |
               substr(ICD10_2, 1, nchar) %in% cod |
               substr(ICD10_3, 1, nchar) %in% cod |
               substr(ICD10_4, 1, nchar) %in% cod |
               substr(ICD10_5, 1, nchar) %in% cod |
               substr(ICD10_6, 1, nchar) %in% cod |
               substr(ICD10_7, 1, nchar) %in% cod |
               substr(ICD10_8, 1, nchar) %in% cod |
               substr(ICD10_9, 1, nchar) %in% cod |
               substr(ICD10_10, 1, nchar) %in% cod |
               substr(ICD10_11, 1, nchar) %in% cod |
               substr(ICD10_12, 1, nchar) %in% cod |
               substr(ICD10_13, 1, nchar) %in% cod |
               substr(ICD10_14, 1, nchar) %in% cod |
               substr(ICD10_15, 1, nchar) %in% cod)
  }
  
  if (!is.null(pod) | all_pod == TRUE) {
    pod_table <- tbl(con, 
                     in_schema(
                       sql(Sys.getenv("BIRTHS_DEATHS_DATABASE")), 
                       sql(Sys.getenv("POD_TABLE")))
    )
    
    pod_lkup <- pod_table %>% 
      mutate(ENTITYCODE = as.integer(Entity),
             DEATHCODE = as.integer(Death_Code)) %>% 
      dplyr::select(ENTITYCODE, DEATHCODE,
                    NHS.INDICATOR = NHS_Ind) %>% 
      left_join(read.csv("data/POD lookup.csv"), by = c("DEATHCODE", "NHS.INDICATOR"),
                copy = TRUE,
                auto_index = TRUE) %>% 
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
      left_join(pod_lkup, by = c("CESTRSS" = "ENTITYCODE"), 
                copy = TRUE,
                auto_index = TRUE) %>%
      left_join(pod_lkup_2, by = c("ESTTYPED" = "DEATHCODE_LKP_2", "NHSIND" = "NHS.INDICATOR"), 
                copy = TRUE,
                auto_index = TRUE) %>%
      mutate(POD_out = ifelse(CESTRSS == "-799", "Home",
                              ifelse(is.na(POD), POD2, POD)))
      
    if (all_pod == FALSE) {
      recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
        filter(POD_out %in% pod_filter)
    }
  } 
  
  lkp <- tbl(con, 
             in_schema(
               sql(Sys.getenv("LOOKUPS_DATABASE")), 
               sql(Sys.getenv("LTLA19_LKP_TABLE")))
             ) %>% 
    rename(LTLAApr19CD = LTLA19CD,
           UTLAApr19CD = UTLA19CD)
  
  utlas <- lkp %>%
    dplyr::select(UTLAApr19CD) %>%
    collect() %>%
    pull()
  
  if (eth_dep == FALSE) {
    recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
      left_join(lkp, by = c("CTYDR" = "LTLAApr19CD")) %>%
      mutate(UTLAApr19CD = ifelse(is.na(UTLAApr19CD) & CTYDR == "E06000060", "E10000002", #New District code introduced into data - Apr 20
                                  ifelse(is.na(UTLAApr19CD) & CTYDR %in% c("E06000061", "E06000062"), "E10000021",
                                         UTLAApr19CD)))
  } else if (eth_dep == TRUE) {


    dep_lkp <- tbl(con, 
                   in_schema(
                     sql(Sys.getenv("LOOKUPS_DATABASE")), 
                     sql(Sys.getenv("LSOA_DEMOGRAPHICS_TABLE")))
                   ) %>%
      dplyr::select(LSOA11CD, IMD2019_Quintiles_LSOA11_England, UTLAApr19CD = UTLA21CD) %>%
      mutate(UTLAApr19CD = ifelse(UTLAApr19CD == "E06000060", "E10000002", #New District code introduced into data - Apr 20
                                  ifelse(UTLAApr19CD %in% c("E06000061", "E06000062"), "E10000021",
                                         UTLAApr19CD)))

    recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
      left_join(dep_lkp, by = c("LSOAR" = "LSOA11CD")) %>%
      rename(Deprivation_Quintile = IMD2019_Quintiles_LSOA11_England)
    
    ethnic_group_names <- ethnic_groups_lookup() %>%
      distinct(Ethnic_Group, Ethnic_group_data_lake) %>% 
      filter(Ethnic_Group != "Unknown") # this removes a one to many join below
    
    eth_lkp <- tbl(con, 
                   in_schema(
                     sql(Sys.getenv("RECENT_DEATHS_DATABASE")), 
                     sql(Sys.getenv("ETHNICITY_LINKED_DEATHS_VIEW_RECENT")))
                   ) %>%
      rename(Ethnic_group_data_lake = NEW_Ethnic_group) %>%
      left_join(ethnic_group_names, by = "Ethnic_group_data_lake", 
                copy = TRUE,
                auto_index = TRUE) %>%
      dplyr::select(Y124_Pseudo_LEDR_ID, Ethnic_Group)
    
    recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
      left_join(eth_lkp, by = "Y124_Pseudo_LEDR_ID") %>%
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
    left_join(agegroup_lkp, by = "Age", 
              copy = TRUE,
              auto_index = TRUE) %>% 
    mutate(DOR = as.Date(DOR),
           # DATEPART = 6 is a Friday
           DOR = ifelse(
             DATEPART(sql("weekday"), DOR) <= 6,
             DATEADD(sql("day"), (6 - DATEPART(sql("weekday"), DOR)), DOR),
             DATEADD(sql("day"), 6, DOR)
           ))
  
  if (eth_dep == TRUE) {
      recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
        group_by(UTLAApr19CD, Deprivation_Quintile, Ethnic_Group, SEX, Age_Group, DOR)
  } else {
    if (all_pod == TRUE) {
      recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
        group_by(UTLAApr19CD, SEX, Age_Group, DOR, POD_out)
    } else {
      recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
        group_by(UTLAApr19CD, SEX, Age_Group, DOR)
    }
  }
 
  recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
    summarise(deaths_total = n(),
              .groups = "drop") %>%
    rename(Reg_Date = DOR,
           Sex = SEX) %>%
    collect() %>%
    ungroup() %>%
    mutate(Reg_Date = as.Date(Reg_Date),
           Sex = as.integer(Sex))
  
  
  # form complete tables with zeros for dates with no deaths
  
  
  if (eth_dep == TRUE) {
      recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
        complete(UTLAApr19CD = utlas,
                 Ethnic_Group = ethnic_groups,
                 Deprivation_Quintile = 1:5,
                 Reg_Date = seq.Date(from = min_date,
                                     to = end_date,
                                     by = "week"),
                 Age_Group,
                 Sex = 1:2,
                 fill = list(deaths_total = 0)) %>%
        arrange(UTLAApr19CD, Ethnic_Group, Deprivation_Quintile, Sex, Age_Group, Reg_Date)
    } else {
      if (all_pod == TRUE) {
        recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
          complete(UTLAApr19CD = utlas,
                   POD_out = POD_out,
                   Reg_Date = seq.Date(from = min_date,
                                       to = end_date,
                                       by = "week"),
                   Age_Group,
                   Sex = 1:2,
                   fill = list(deaths_total = 0)) %>%
          arrange(UTLAApr19CD, POD_out, Sex, Age_Group, Reg_Date)
      } else {
        recent_deaths_lower_geographies <- recent_deaths_lower_geographies %>%
          complete(UTLAApr19CD = utlas,
                   Reg_Date = seq.Date(from = min_date,
                                       to = end_date,
                                       by = "week"),
                   Age_Group,
                   Sex = 1:2,
                   fill = list(deaths_total = 0)) %>%
          arrange(UTLAApr19CD, Sex, Age_Group, Reg_Date)
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
                                            deaths_field, region_field, age_field, sex_field, date_field) {
  ethnicity_groups <- c("Asian", "Black", "Mixed", "Other", "White")
  
  copy_proportions <- !is.null(proportions)
  if (is.null(proportions)) proportions <- calculate_ethnicity_proportions(data, {{ ethnicity_field }}, 
                                                                           {{ region_field }}, {{ age_field }}, 
                                                                           {{ sex_field }}, {{ deaths_field }})
  
  group_columns <- c(rlang::as_label(enquo(region_field)),
                     rlang::as_label(enquo(age_field)),
                     rlang::as_label(enquo(sex_field)), 
                     "Deprivation_Quintile",
                     rlang::as_label(enquo(date_field)))
  
  select_columns <- c(group_columns,
                      rlang::as_label(enquo(ethnicity_field)),
                      "Deprivation_Quintile",
                      rlang::as_label(enquo(date_field)),
                      "unknown_deaths")
  
  unknown_deaths <- data %>%
    filter(!({{ ethnicity_field }} %in% ethnicity_groups)) %>%
    group_by(across(all_of(group_columns))) %>%
    summarise(summed_deaths = sum({{ deaths_field }}), 
              .groups = "drop") %>%
    left_join(proportions, by = intersect(names(.), names(proportions)), copy = copy_proportions) %>%
    mutate(unknown_deaths = summed_deaths * proportion) %>%
    dplyr::select(all_of(select_columns))
  
  recalculated_data <- data %>%
    filter({{ ethnicity_field }} %in% ethnicity_groups) %>%
    left_join(unknown_deaths, by = intersect(names(.), names(unknown_deaths))) %>%
    mutate({{ deaths_field }} := {{ deaths_field }} + unknown_deaths) %>%
    dplyr::select(-unknown_deaths)
  
  
  return(recalculated_data)
}

#' @description function to calculate the proportion of deaths in each ethnic group for a given day, location, 
#'   age group and sex
#' @inheritParams ethnicity_not_stated_adjustment
calculate_ethnicity_proportions <- function(data, ethnicity_field, region_field, age_field, 
                                            sex_field, deaths_field, 
                                            by_date = FALSE, look_back_days = 30) {
  
  ethnicity_groups <- c("Asian", "Black", "Mixed", "Other", "White")
  grouping_fields_2 <- c(rlang::as_label(enquo(region_field)),
                         rlang::as_label(enquo(age_field)),
                         rlang::as_label(enquo(sex_field)), 
                         "Deprivation_Quintile")
  grouping_fields_1 <- c(rlang::as_label(enquo(region_field)),
                         rlang::as_label(enquo(age_field)),
                         rlang::as_label(enquo(sex_field)), 
                         rlang::as_label(enquo(ethnicity_field)),
                         "Deprivation_Quintile")
  
  if (by_date == TRUE) {
    grouping_fields_1 <- c(grouping_fields_1, "Reg_Date")
    grouping_fields_2 <- c(grouping_fields_2, "Reg_Date")
  }
  
  proportions <- data %>%
    filter({{ ethnicity_field }} %in% ethnicity_groups) %>%
    group_by(across(all_of(grouping_fields_1))) %>%
    summarise(summed_deaths := sum({{ deaths_field }}), .groups = "drop_last")
  
  if (by_date == TRUE) {
    # calculate the proportion of deaths over the whole time period in each subgroup
    whole_time_period_proportions <- proportions %>% 
      ungroup() %>% 
      group_by(across(all_of(grouping_fields_1[grouping_fields_1 != "Reg_Date"]))) %>% 
      summarise(summed_deaths = sum(summed_deaths),
                .groups = "drop") %>% 
      group_by(across(all_of(grouping_fields_2[grouping_fields_2 != "Reg_Date"]))) %>% 
      mutate(proportion_whole_time_period = summed_deaths / sum(summed_deaths)) %>%
      ungroup() %>%
      dplyr::select(-summed_deaths)
    
    # Calculate the mean number of deaths in each subgroup over the previous "look_back_days" period
    proportions <- proportions %>% 
      mutate(summed_deaths = slide_index_dbl(.x = summed_deaths,
                                            .i = Reg_Date, 
                                            .f = mean, 
                                            .before = look_back_days, 
                                            .complete = FALSE)) %>% 
      ungroup() %>% 
      group_by(across(all_of(grouping_fields_2))) %>% 
    # Use this mean number to calculate the proportion of deaths in each subgroup
      mutate(proportion = summed_deaths / sum(summed_deaths)) %>%
      ungroup() %>%
      dplyr::select(-summed_deaths) %>% 
    # where there were no deaths over the "look_back_days" time period in a subgroup, apply the proportions from the whole time period
      left_join(whole_time_period_proportions, by = intersect(names(.), names(whole_time_period_proportions))) %>% 
      mutate(proportion = case_when(
        is.na(proportion) ~ proportion_whole_time_period,
        TRUE ~ proportion
      )) %>% 
      dplyr::select(-proportion_whole_time_period)
    
  } else if (by_date == FALSE) {
    
    proportions <- proportions %>% 
      ungroup() %>% 
      group_by(across(all_of(grouping_fields_2))) %>% 
      mutate(proportion = summed_deaths / sum(summed_deaths)) %>%
      ungroup() %>%
      dplyr::select(-summed_deaths)
  }
  
  return(proportions)
}
