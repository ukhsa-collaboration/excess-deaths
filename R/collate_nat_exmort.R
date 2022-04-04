# Place the datasets to be collated in a folder named 'input' in the project

# functions
# additional calcs
# 
collate_powerbi_files_for_powerbi <- function(geography = "england", 
                                              final_date,
                                              report_type = "live") {
  
  if (!(geography %in% c("england", "region"))) stop("geography must be one of 'region' or 'england'")
  
  rgn_part <- ""
  if (geography == "region") rgn_part <- "_RGN09CD"
  
  suffix <- "_ons_aligned_weekly"
  
  live_or_test_suffix <- ""
  if (report_type == "test") live_or_test_suffix = "_test"
  
  suffix <- paste0(suffix, 
                   live_or_test_suffix)
  
  f_calc <- function(df, ucod_breakdown = FALSE) {
    
    df <- df %>%
      dplyr::select(everything(), covid_dths = covid, all_dths = registered, exptd_dths = expected) %>% 
      
      group_by(Ethnic_Group, Sex, type, RGN09CD, RGN09NM, Age_Group, name_of_cause, POD_out,
               Deprivation_Quintile, days_in_week) %>%
      
      mutate(
        # cumulative values
        cuml_covid_dths = cumsum(covid_dths), 
        cuml_all_dths = cumsum(all_dths),
        cuml_exptd_dths = cumsum(exptd_dths),
        
        # other values (tot_excess + exptd_dths = all_dths)
        excess_deficit = case_when(
                  all_dths < exptd_dths ~ all_dths - exptd_dths, 
                  TRUE ~ as.numeric(0)),
        tot_excess = case_when(
          all_dths > exptd_dths ~ all_dths - exptd_dths,
          TRUE ~ as.numeric(0))) %>%
      ungroup()
    
    return(df)
  }
  
  
  f_chng_dep <- function(df) {
    str_replace_all(df$Deprivation_Quintile, c("Quintile 1 - most deprived" = "IMD 1",
                                               "Quintile 2" = "IMD 2",
                                               "Quintile 3" = "IMD 3",
                                               "Quintile 4" = "IMD 4",
                                               "Quintile 5 - least deprived" = "IMD 5"))
  }
  
  
  f_typeall  <- function(df, type) {
    
    type <- rlang::enquo(type)
    df %>%
      pivot_longer(cols = registered:covid, names_to = "data_type", values_to = "count") %>%
      group_by_at(vars(-!!type, -count)) %>%
      summarise(count = sum(count),
                .groups = "drop") %>%
      pivot_wider(names_from = "data_type", values_from = "count") %>%
      relocate(days_in_week, .after = last_col())
  }
  
  deprivation_function <- function(data_filepath, type_name, age_group_name) {
    df <- read.csv(data_filepath) %>% 
      adjust_rgn_fields(geography = geography,
                        rgn_lkp = lkup_rgn) %>% 
      tibble::add_column(type    = type_name,
                         Ethnic_Group  = "All",
                         Age_Group     = age_group_name,
                         name_of_cause = "All",
                         POD_out = "All",
                         .before = "date"
      ) 
    
    # Sums the deprivation_quintile field to create an "All" field
    df_all <- f_typeall(df, "Deprivation_Quintile") %>%
      tibble::add_column(Deprivation_Quintile = "All", .before = 1)
    
    df <-  bind_rows(df, df_all) 
    
    # Sums the sex field to create a "Persons group" value for each week/dep group
    df_dep_p   <- f_typeall(df, "Sex") %>%
      tibble::add_column(Sex = "Persons", .before = 1)
    
    df <-  bind_rows(df, df_dep_p) %>%
      f_calc() %>%
      dplyr::select(type, RGN09CD, RGN09NM, Sex, Ethnic_Group, Age_Group, 
                    name_of_cause, POD_out, Deprivation_Quintile, date, 
                    days_in_week, all_dths, exptd_dths, covid_dths, 
                    everything())
    
    df$Deprivation_Quintile <- f_chng_dep(df)
    
    df <- df %>%
      mutate(Plot_Label = Deprivation_Quintile)
    
    return(df)
  }
  
  # region lookup
  utla_lkp <- utla_lookup()
  lkup_rgn   <- utla_lkp %>% 
    distinct(RGN09CD, RGN09NM)
  lkup_utla  <- utla_lkp %>% 
    dplyr::select(UTLA19CD = UTLAApr19CD, UTLA19NM = UTLAApr19NM)
  
  # region field adjustment if depending on geography
  adjust_rgn_fields <- function(data, geography, rgn_lkp) {
    if (geography == "england") {
      data <- data %>% 
        tibble::add_column(RGN09CD = "E92000001", 
                           RGN09NM = "England")
    } else if (geography == "region") {
      data <- data %>% 
        left_join(rgn_lkp, by = "RGN09CD")
    }
    return(data)
  }
  
  
  folder_path <- Sys.getenv("POWERBI_FILESHARE")
  
  # load separate files, add columns to match and merge
  # 
  # England
  df_eng    <- read.csv(
    paste0(folder_path,"/", 
           all_models("Ethnicity-deprivation"), 
           rgn_part,
           suffix, ".csv")
  ) %>% 
    adjust_rgn_fields(geography = geography,
                      rgn_lkp = lkup_rgn) %>% 
    tibble::add_column(type    = "England",
                       Sex     = "Persons", 
                       Ethnic_Group  = "All",
                       Age_Group     = "All",
                       name_of_cause = "All",
                       POD_out       = "All",
                       Deprivation_Quintile = "All",
                       .before = "date"
    ) %>% 
    f_calc() %>%
    dplyr::select(type, RGN09CD, RGN09NM, Sex, Ethnic_Group, Age_Group, name_of_cause, 
                  POD_out, Deprivation_Quintile, date, days_in_week, all_dths, 
                  exptd_dths, covid_dths, everything()
    )
  
  if (geography == "england") {
    # Region
    df_rgn    <- read.csv(
      paste0(folder_path, "/", 
             all_models("Ethnicity-deprivation"), 
             "_RGN09CD",
             suffix, 
             ".csv")
    ) %>%
      left_join(lkup_rgn, by = "RGN09CD") %>%  
      tibble::add_column(type = "region",
                         Sex  = "Persons", 
                         Ethnic_Group  = "All",
                         Age_Group     = "All",
                         name_of_cause = "All",
                         POD_out       = "All",
                         Deprivation_Quintile = "All",
                         .before = "date"
      ) %>% 
      f_calc() %>%
      dplyr::select(type, RGN09CD, RGN09NM, Sex, Ethnic_Group, Age_Group, name_of_cause, 
                    POD_out, Deprivation_Quintile, date, days_in_week, all_dths, 
                    exptd_dths, covid_dths, everything()
      )
    
    df_rgn <- bind_rows(df_rgn, df_eng)
    
    df_rgn$type    <- str_replace_all(df_rgn$type, "England", "region")
    df_rgn$RGN09NM <- str_replace_all(df_rgn$RGN09NM, c("Yorkshire and The Humber" = "Yorkshire & Humber",
                                                        "England" = " England"))
    df_rgn <- df_rgn %>%
      mutate(Plot_Label = RGN09NM)
    
    
    
  }
  
  # UTLA
  df_utla  <- read.csv(
    paste0(folder_path, "/", 
           all_models("UTLA"), 
           rgn_part,
           "_UTLAApr19CD",
           suffix, 
           ".csv")
  ) %>%
    left_join(lkup_utla, by = c("UTLAApr19CD" = "UTLA19CD")) 
  
  if (geography == "region") {
    df_utla <- df_utla %>% 
      dplyr::select(!c(RGN09CD))
  }
  
  df_utla <- df_utla %>% 
    dplyr::select(RGN09CD = UTLAApr19CD, RGN09NM = UTLA19NM, everything()) %>%
    tibble::add_column(type = "utla",
                       Sex  = "Persons", 
                       Ethnic_Group  = "All",
                       Age_Group     = "All",
                       name_of_cause = "All",
                       POD_out       = "All",
                       Deprivation_Quintile = "All",
                       .before = "date"
    ) %>% 
    f_calc() %>%
    dplyr::select(type, RGN09CD, RGN09NM, Sex, Ethnic_Group, Age_Group, name_of_cause, 
                  POD_out, Deprivation_Quintile, date, days_in_week, all_dths, 
                  exptd_dths, covid_dths, everything()
    ) %>%
    mutate(Plot_Label = RGN09NM)
  
  # Age_Group
  df_agegrp   <- read.csv(
    paste0(folder_path, "/", 
           all_models("Ethnicity-deprivation"), 
           rgn_part,
           "_Age_Group_Sex",
           suffix, 
           ".csv")
  ) %>% 
    adjust_rgn_fields(geography = geography,
                      rgn_lkp = lkup_rgn) %>% 
    tibble::add_column(type    = "Age_Group",
                       Ethnic_Group  = "All",
                       name_of_cause = "All",
                       POD_out       = "All",
                       Deprivation_Quintile = "All", 
                       .before = "date"
    ) 
  
  df_agegrp_p <- f_typeall(df_agegrp, "Sex") %>%
    tibble::add_column(Sex = "Persons", .before = 1)
  
  df_agegrp   <- bind_rows(df_agegrp, df_agegrp_p) %>%
    f_calc() %>%
    dplyr::select(type, RGN09CD, RGN09NM, Sex, Ethnic_Group, Age_Group, 
                  name_of_cause, POD_out, Deprivation_Quintile, date, 
                  days_in_week, all_dths, exptd_dths, covid_dths, 
                  everything()
    )
  
  df_agegrp$Age_Group <- df_agegrp$Age_Group %>% str_replace_all("Total", " All")
  
  df_agegrp <- df_agegrp %>%
    mutate(Plot_Label = Age_Group)
  
  # Ethnicity
  df_ethnic <- read.csv(
    paste0(folder_path, "/", 
           all_models("Ethnicity-deprivation"), 
           rgn_part,
           "_Ethnic_Group_Sex",
           suffix, 
           ".csv")
  ) %>% 
    adjust_rgn_fields(geography = geography,
                      rgn_lkp = lkup_rgn) %>% 
    tibble::add_column(type      = "Ethnicity",
                       Age_Group = "All",
                       name_of_cause = "All",
                       POD_out       = "All",
                       Deprivation_Quintile = "All",
                       .before = "date"
    )
  
  # Sums the ethnic groups to create an "All ethnic group" value for each week/sex
  df_ethnic_all <- f_typeall(df_ethnic, "Ethnic_Group") %>%
    tibble::add_column(Ethnic_Group = "All", .before = 1)
  
  df_ethnic     <- bind_rows(df_ethnic, df_ethnic_all)
  
  # Sums the sex field to create a "Persons group" value for each week/eth group
  df_ethnic_p   <- f_typeall(df_ethnic, "Sex") %>%
    tibble::add_column(Sex = "Persons", .before = 1)
  
  df_ethnic <-  bind_rows(df_ethnic, df_ethnic_p) %>%
    f_calc() %>%
    dplyr::select(type, RGN09CD, RGN09NM, Sex, Ethnic_Group, Age_Group, 
                  name_of_cause, POD_out, Deprivation_Quintile, date, 
                  days_in_week, all_dths, exptd_dths, covid_dths, 
                  everything()
    ) %>%
    mutate(Plot_Label = Ethnic_Group)
  
  
  # Place of death1
  pod_rgn_part <- ""
  if (rgn_part != "") pod_rgn_part <- "RGN09CD_"
  df_pod    <- read.csv(
    paste0(folder_path, "/",
           pod_rgn_part,
           "POD_out", 
           suffix, ".csv")
  )  %>% 
    adjust_rgn_fields(geography = geography,
                      rgn_lkp = lkup_rgn) %>% 
    tibble::add_column(type    = "pod",
                       Sex     = "Persons", 
                       Ethnic_Group  = "All",
                       Age_Group     = "All",
                       name_of_cause = "All",
                       Deprivation_Quintile = "All",
                       .before = "date"
    ) 
  
  df_pod_all <- f_typeall(df_pod, "POD_out") %>%
    tibble::add_column(POD_out = "All", .before = 1)
  
  df_pod <-  bind_rows(df_pod, df_pod_all) %>%
    f_calc() %>%
    dplyr::select(type, RGN09CD, RGN09NM, Sex, Ethnic_Group, Age_Group, 
                  name_of_cause, POD_out, Deprivation_Quintile, date, 
                  days_in_week, all_dths, exptd_dths, covid_dths, 
                  everything()
    ) %>%
    mutate(Plot_Label = POD_out)
  
  
  # Deprivation all ages
  df_dep <- paste0(folder_path, "/",
                      all_models("Ethnicity-deprivation"),
                      rgn_part,
                      "_Deprivation_Quintile_Sex",
                      suffix,
                      ".csv") %>%
    deprivation_function(
      type_name = "deprivation",
      age_group_name = "All")
  
  
 # Cause of death mentions
  
  causes <- cause_code_lookup(table_output = TRUE,
                              include_epi_hf = TRUE) %>%
    filter(name_of_cause != "COVID-19") %>%
    mutate(name_of_cause = capitalise_first_letter(name_of_cause)) %>% 
    pull(name_of_cause) %>% 
    unique()
  
  df_cause <- model_references() %>%
    filter(reference %in% causes) %>%
    dplyr::select(reference, model_file) %>% 
    mutate(model_file = gsub("model_outputs\\\\", "", model_file),
           model_file = gsub(".rds", "", model_file),
           model_file = paste0(folder_path,
                               "/", 
                               model_file, 
                               rgn_part,
                               suffix, ".csv")) %>% 
    tibble::deframe() %>% 
    purrr::map_df(~ read.csv(.x),
                  .id = "name_of_cause") %>% 
    adjust_rgn_fields(geography = geography,
                      rgn_lkp = lkup_rgn) %>% 
    tibble::add_column(type    = "all cause",
                       Sex     = "Persons", 
                       Ethnic_Group = "All",
                       Age_Group    = "All",
                       POD_out      = "All",
                       Deprivation_Quintile = "All",
                       covid = NA_real_,
                       .before = "date"
    ) %>%
    # rename(covid = ucod_covid) %>% 
    f_calc() %>% 
    # rename(ucod_covid = covid_dths) %>% 
    mutate(Plot_Label = name_of_cause)
  
  
  if (geography == "england") {
    df_nat_exdeaths <- bind_rows(
      df_eng, df_utla, df_agegrp, df_ethnic, #df_ari, df_dement, df_dm, 
      # df_depu75, df_dep75o, df_depeth, 
      df_dep, df_rgn, df_cause, df_pod
    )  
  } else if (geography == "region") {
    df_nat_exdeaths <- bind_rows(
      df_eng, df_utla, df_agegrp, df_ethnic, #df_ari, df_dement, df_dm, 
      # df_depu75, df_dep75o, df_depeth, 
      df_dep, df_cause, df_pod
      ) 
  }


  # Add dispersion parameters 
  dispersion_parameters <- obtain_dispersion_parameters() %>% 
    mutate(Chart_Name = gsub(" mentions$", "", Chart_Name))
  
  
  # Make lookup between dispersion parameters and excess mortality data
  lookup <- distinct(df_nat_exdeaths, type, name_of_cause, POD_out) %>%
    mutate(
      Chart_Name = case_when(
        type == "all cause" ~ name_of_cause,
        type == "pod" ~ POD_out,
        type == "Diabetes mellitus" ~ "Diabetes mentions",
        type == "Acute respiratory infections" ~ "ARI mentions",
        type == "Dementia" ~ "Dementia mentions",
        type == "region" ~ "Region",
        type == "Age_Group" ~ "Age-Sex",
        type %in% c("dep_o75", "dep_u75", "deprivation") ~ "Deprivation",
        type == "Ethnicity" ~ "Ethnicity-Sex",
        type == "England" ~ "England",
        type == "utla" ~ "UTLA",
        type == "dep_ethnicity" ~ "Ethnicity-deprivation",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(Chart_Name != "All")
  
  
  df_nat_exdeaths <- df_nat_exdeaths %>% 
    left_join(lookup, by = c("type", "name_of_cause", "POD_out")) %>%
    left_join(dispersion_parameters, by = c("Chart_Name")) %>%
    mutate(across(all_of(c("ucod_disease", 
                           "ucod_covid", 
                           "covid_dths", "cuml_covid_dths")),
                  ~ case_when(
                      is.na(.x) ~ "",
                      TRUE ~ as.character(.x)))) %>% 
    filter(!(type %in% c("dep_u75", "dep_o75", "dep_ethnicity")),
           name_of_cause != "Epilepsy")
  
  # check that final date for each group is the expected final date
  check_max_date <- df_nat_exdeaths %>% 
    mutate(date = as.Date(date)) %>% 
    group_by(type) %>% 
    filter(date == max(date)) %>% 
    mutate(date = case_when(
      type == "Ethnicity" ~ date + 7,
      TRUE ~ date
    )) %>% 
    filter(date != final_date) %>% 
    pull(type) %>% 
    unique()
  
  if (length(check_max_date) > 0) 
    stop(paste("The final week for the following chart types is not the expected final week;",
               paste(check_max_date, collapse = ", ")))
  
  # Write to file Note, this isn't the final file if this is the regional
  # process - but needs to be written here because the National file is produced
  # from this one
  output_filepath <- paste0(Sys.getenv("POWERBI_FILESHARE"),
                            "/nat_exdeaths_pbi_",
                            geography,
                            "_",
                            format(final_date, "%Y%m%d"),
                            live_or_test_suffix,
                            ".csv")
  
  # save NAs to blank so 'covid_dths', ucod_covid' and 'ucod_disease' columns do
  # not produce errors in the pbix
  write_csv(df_nat_exdeaths, output_filepath, na = "") 
  
  
  if (geography == "region") {
    # output aggregated regional data for national PowerBI
    region_aggregated <- aggregate_regional_to_national(output_filepath)
    
    region_aggregated_output_filepath <- paste0(Sys.getenv("POWERBI_FILESHARE"),
                                                "/nat_exdeaths_pbi_england_", 
                                                format(final_date, "%Y%m%d"),
                                                live_or_test_suffix,
                                                ".csv")
    write_csv(region_aggregated, region_aggregated_output_filepath, na = "")
    
    # Create combined regional and aggregated regional dataset for regional PowerBI
    df_nat_exdeaths <- df_nat_exdeaths %>% 
       mutate(type = case_when(type == "England" ~ "region",  # for regional data, change type from 'England' to 'region'
                               TRUE ~ type))
    
    # manipulate national dataset
    region_aggregated <- region_aggregated %>%
      mutate(RGN09CD = "E92000001")  %>% # change RGN09CD for national ulta data to England's code
      filter(!(type %in% c("England"))) %>% # drop rows for type 'England'
      filter(!(type == "region" & RGN09NM != " England")) %>% # drop rows for type 'region' except where RGN09NM is ' England'
      mutate(RGN09NM = stringr::str_trim(RGN09NM, side = "left"), # remove trailing space in ' England' in RGN09NM and Plot_Label columns
             Plot_Label = stringr::str_trim(Plot_Label, side = "left"))
    
    # change column data types from character to numeric, so regional and region_aggregated datasets can be joined
    cols_to_numeric <- c("covid_dths", "cuml_covid_dths", "ucod_covid", "ucod_disease")
    df_nat_exdeaths[cols_to_numeric] <- sapply(df_nat_exdeaths[cols_to_numeric], as.numeric)
    
    # combine regional and national data
    df_combined_exdeaths <- df_nat_exdeaths %>%
      bind_rows(region_aggregated) %>% # Create region_dropdown column
      left_join(utla_lkp %>%
                  select(UTLAApr19CD, region_dropdown = RGN09NM),
                by = c("RGN09CD" = "UTLAApr19CD")
                ) %>%
      mutate(region_dropdown = case_when(str_sub(RGN09CD, 1, 3) == "E12" ~ RGN09NM,
                                          str_sub(RGN09CD, 1, 3) == "E92" ~ "England",
                                          TRUE ~ region_dropdown
                                          )
             ) %>% # add region_sort_order column
      mutate(region_sort_order = case_when(str_sub(RGN09CD, 1, 3) == "E92" ~ as.numeric(1),
                                            region_dropdown == "East Midlands" ~ as.numeric(2),
                                            region_dropdown == "East of England" ~ as.numeric(3),
                                            region_dropdown == "London" ~ as.numeric(4),
                                            region_dropdown == "North East" ~ as.numeric(5),
                                            region_dropdown == "North West" ~ as.numeric(6),
                                            region_dropdown == "South East" ~ as.numeric(7),
                                            region_dropdown == "South West" ~ as.numeric(8),
                                            region_dropdown == "West Midlands" ~ as.numeric(9),
                                            region_dropdown == "Yorkshire and The Humber" ~ as.numeric(10)
                                            )
             )

    output_filepath <- paste0(Sys.getenv("POWERBI_FILESHARE"), 
                              "/nat_exdeaths_pbi_", 
                              geography,
                              "_",
                              format(final_date, "%Y%m%d"),
                              live_or_test_suffix,
                              ".csv")
    
    write_csv(df_combined_exdeaths, 
              output_filepath, 
              na = "") # save NAs to blank so 'ucod_covid' and 'ucod_disease' columns do not produce errors in the pbix
  }
  
  
  if (geography == "england") {
    copied <- file.copy(from = output_filepath,
                        to = paste0(Sys.getenv("LKIS_FILESHARE"),
                                    "nat_exdeaths_pbi_sf",
                                    live_or_test_suffix,
                                    ".csv"),
                        overwrite = TRUE)

    if (copied == TRUE) {
      print("The Power BI data has been copied to the LKIS server. The Power BI tool now needs refreshing.")
    } else {
      print("Something has gone wrong with copying the file to the LKIS server for Power BI to read in.")
      }
  } else if (geography == "region") {
      copied <- file.copy(from = region_aggregated_output_filepath,
                          to = paste0(Sys.getenv("LKIS_FILESHARE"),
                                      "nat_exdeaths_pbi_sf",
                                      live_or_test_suffix,
                                      ".csv"),
                          overwrite = TRUE)           
      copied <- file.copy(from = output_filepath,
                          to = paste0(Sys.getenv("LKIS_FILESHARE"),
                                      "reg_exdeaths_pbi_sf",
                                      live_or_test_suffix,
                                      ".csv"),
                          overwrite = TRUE)
      if (copied == TRUE) {
        print("The Power BI data has been copied to the LKIS server. The Power BI tool now needs refreshing.")
      } else {
        print("Something has gone wrong with copying the file to the LKIS server for Power BI to read in.")
        }
    }
      
  
  return(output_filepath)
  
}
