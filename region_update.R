source("R/libraries.R")
source("R/function_predictions.R")
source("R/function_visualisations.R")
source("R/utils.R")
source("R/function_monthly_populations.R")
source("tests/test-assertr.R")
source("R/function_deaths_data.R")
source("R/collate_nat_exmort.R")

# run next line the Friday before the report update 
check_ethnicity_linkage()

# continue from here for report update
memory.limit(32698)

final_date <- final_report_date()

ethnic_group_final_date <- final_date - 7

eth_dep_setting <- TRUE
age_group_type_setting <- "nomis"
pop_type_setting <- "estimates"
from_date <- as.Date("2020-03-21")
# report_type_setting <- "test"
report_type_setting <- "live"

live_or_test_suffix <- ""
if (report_type_setting == "test") live_or_test_suffix <- "_test"

rgn_charts <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                      visualisation_geography = "region",
                                      end_date = final_date, 
                                      from_date = from_date,
                                      eth_dep = eth_dep_setting, 
                                      age_group_type = age_group_type_setting,
                                      report_type = report_type_setting)

rgn_as_charts <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                         visualisation_geography = "region",
                                         facet_fields = c("Age_Group", "Sex"),
                                         split_chart = "Sex",
                                         eth_dep = eth_dep_setting, 
                                         end_date = final_date, 
                                         from_date = from_date,
                                         include_totals = TRUE,
                                         age_group_type = age_group_type_setting,
                                         report_type = report_type_setting)

utla_charts <- generate_visualisations(paste0("model_outputs/", all_models("UTLA"), ".rds"),
                                       visualisation_geography = "region", 
                                       facet_fields = "UTLAApr19CD",
                                       split_chart = "UTLAApr19CD",
                                       end_date = final_date,
                                       from_date = from_date,
                                       report_type = report_type_setting)

#### ETHNICITY CHARTS ####
rgn_ethnicity_sex <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                             visualisation_geography = "region",
                                             facet_fields = c("Ethnic_Group", "Sex"), 
                                             split_chart = "Sex",
                                             end_date = ethnic_group_final_date, 
                                             from_date = from_date,
                                             eth_dep = eth_dep_setting, 
                                             age_group_type = age_group_type_setting,
                                             report_type = report_type_setting)

### DEPRIVATION BY SEX CHARTS ####
rgn_deprivation_sex <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                              visualisation_geography = "region",
                                              facet_fields = c("Deprivation_Quintile", "Sex"),
                                              split_chart = "Sex",
                                              end_date = final_date,
                                              from_date = from_date,
                                              eth_dep = eth_dep_setting,
                                              age_group_type = age_group_type_setting,
                                              report_type = report_type_setting)

#### MORTALITY BY CAUSE OF DEATH ####

# all diseases compared
causes <- cause_code_lookup(table_output = TRUE,
                            include_epi_hf = TRUE) %>%
  filter(name_of_cause != "COVID-19") %>%
  mutate(name_of_cause = capitalise_first_letter(name_of_cause)) %>% 
  group_by(name_of_cause) %>% 
  summarise(codes = list(ICD_codes),
            .groups = "drop")

model_files <- model_references() %>%
  filter(reference %in% causes$name_of_cause) %>%
  dplyr::select(reference, model_file) %>% 
  left_join(causes, by = c("reference" = "name_of_cause")) %>% 
  mutate(model_file = gsub("\\\\", "/", model_file))

all_mentions_data <- model_files %>%
  group_by(reference) %>% 
  group_split() %>% 
  purrr::map(~ generate_visualisations(ucods = .x[["codes"]][[1]],
                                       cod = .x[["codes"]][[1]],
                                       model_filename = .x[["model_file"]],
                                       end_date = final_date,
                                       from_date = from_date,
                                       visualisation_geography = "region",
                                       report_type = report_type_setting))

#### MORTALITY BY PLACE OF DEATH ####
# All places of death combined
pods <- pod_lookup() %>%
  pull(pod_filter) %>%
  unique()
model_files <- model_references() %>%
  filter(reference %in% pods) %>%
  dplyr::select(reference, model_file) %>% 
  tibble::deframe()

rgn_all_pods <- generate_visualisations(model_filename = model_files,
                                        all_pod = TRUE,
                                        end_date = final_date, 
                                        from_date = from_date,
                                        eth_dep = FALSE,
                                        visualisation_geography = "region",
                                        facet_fields = "POD_out",
                                        split_chart = "POD_out",
                                        report_type = report_type_setting)

mysubtitle <- "Experimental Statistics"

icd_references <- model_references() %>% 
  filter(!is.na(caption),
         !is.na(reference)) %>% 
  mutate(caption = gsub("ICD10 reference: ", "", caption),
         reference = capitalise_first_letter(reference)) %>% 
  dplyr::select(`Cause description` = reference,
                `ICD10 reference` = caption) %>% 
  map_df(rev)

# this steps creates both the national and the regional data
path_region <- collate_powerbi_files_for_powerbi(geography = "region", 
                                                 final_date = final_date,
                                                 report_type = report_type_setting)

path_national <- gsub("region", "england", path_region)

generate_regional_date_csv_lookup()


# QA the numbers ----------------------------------------------------------
# bear in mind, all persons, age group, region, ethnicity-sex, deprivation-sex are all the same model
# this table shows the difference between the cumulative all persons deaths (registered, expected and covid) 
# compared to the totals within each subpopulation, and also the subgroups within each 
# subpopulation aggregated (ie, each place of death added together).
# The table is filtered for comparisons where any of registered, expected or covid deaths aren't 0

qa_region <- qa_power_bi_file(path_region,
                              
                       geography = "region") %>% 
  filter(all_dths != 0 |
           exptd_dths != 0 |
           covid_dths != 0)


if (!Sys.getenv("USERNAME") %in% c("sebastian.fox", "sam.dunn")) View(qa_region) # Add in not in

write.csv(qa_region,
          paste0(Sys.getenv("POWERBI_FILESHARE"),
                 "/qa/region_compared_to_all_persons_",
                 gsub("-", "", as.character(final_date)),
                 live_or_test_suffix,
                 ".csv"),
          row.names = FALSE)

qa_national <- qa_power_bi_file(path_national)

if (!Sys.getenv("USERNAME") %in% c("sebastian.fox", "sam.dunn")) View(qa_national) # Add in not in

write.csv(qa_national,
          paste0(Sys.getenv("POWERBI_FILESHARE"),
                 "/qa/compared_to_all_persons_",
                 gsub("-", "", as.character(final_date)),
                 live_or_test_suffix,
                 ".csv"),
          row.names = FALSE)

# a function that will remove the aggregated England records from the regional
# powerbi csv file so the regional file can be compared like-for-like with the
# national file
remove_aggregated_england_records <- function(filename) {
  if (grepl("region", filename)) {
    data <- read.csv(filename) %>% 
      filter(RGN09CD != "E92000001")
  } else {
    data <- read.csv(filename)
  }
  return(data)
}

# this checks the totals for the regional reports for the mentions charts 
# against the equivalent totals in the national report
compare_mentions_region_national <- list(region = path_region,
                                         national = path_national) %>% 
  lapply(function(x) x %>% 
           remove_aggregated_england_records() %>% 
           filter(type == "all cause") %>% 
           group_by(Chart_Name) %>% 
           summarise(across(c(all_dths, exptd_dths, ucod_covid, ucod_disease),
                            sum),
                     .groups = "drop") %>% 
           tidyr::pivot_longer(cols = !c(Chart_Name),
                               names_to = "type",
                               values_to = "val")) %>% 
  reduce(left_join, by = c("Chart_Name", "type")) %>% 
  mutate(region_minus_national = val.x - val.y) %>% 
  dplyr::select(!c(val.x, val.y)) %>% 
  arrange(desc(abs(region_minus_national)))

if (!Sys.getenv("USERNAME") %in% c("sebastian.fox", "sam.dunn")) View(compare_mentions_region_national)

write.csv(compare_mentions_region_national,
          paste0(Sys.getenv("POWERBI_FILESHARE"),
                 "/qa/region_vs_national_mentions_compared_",
                 gsub("-", "", as.character(final_date)),
                 live_or_test_suffix,
                 ".csv"),
          row.names = FALSE)

# This checks this month's data with what was published last month
# Bear in mind that cause of death is updated on death certificate over time
compared_to_last_month <- compare_this_and_last_weeks_file(path_region,
                                                           geography = "region")

if (!Sys.getenv("USERNAME") %in% c("sebastian.fox", "sam.dunn")) View(compared_to_last_month) # add in not in

write.csv(compared_to_last_month,
          paste0(Sys.getenv("POWERBI_FILESHARE"),
                 "/qa/region_compared_to_last_month_",
                 gsub("-", "", as.character(final_date)),
                 live_or_test_suffix,
                 ".csv"),
          row.names = FALSE)

# This checks this week's national data with what was published last week
compared_to_last_week <- compare_this_and_last_weeks_file(path_national)

if (!Sys.getenv("USERNAME") %in% c("sebastian.fox", "sam.dunn")) View(compared_to_last_week) # add in not in

write.csv(compared_to_last_week,
          paste0(Sys.getenv("POWERBI_FILESHARE"),
                 "/qa/compared_to_last_week_",
                 gsub("-", "", as.character(final_date)),
                 live_or_test_suffix,
                 ".csv"),
          row.names = FALSE)

# Create an excel file containing all of the data for public use --------

xlsx_file_region <- create_excel_file(input_filepath = path_region,
                                      output_filepath = paste0(Sys.getenv("POWERBI_FILESHARE"),
                                                               "/RegionalEMData", 
                                                               live_or_test_suffix,
                                                               ".xlsx"),
                                      geography = "region")

xlsx_file_national <- create_excel_file(input_filepath = path_national,
                                        output_filepath = paste0(Sys.getenv("POWERBI_FILESHARE"),
                                                                 "/EMData", 
                                                                 live_or_test_suffix,
                                                                 ".xlsx"),
                                        geography = "england")


# Install RDCOMClient from binaries ---------------------------------------

if (!require(RDCOMClient)) {
  #url <- "http://www.omegahat.net/R/bin/windows/contrib/4.0.0/RDCOMClient_0.94-0.zip"
  #install.packages(url, repos = NULL, type = "binary")
  devtools::install_github("dkyleward/RDCOMClient") # could create snap shot with this. - library command line 98
}
detach("package:RDCOMClient", unload = TRUE)

if (Sys.getenv("USERNAME") == "sam.dunn") library(RDCOMClient, lib.loc = paste0("C:/Users/",Sys.getenv("USERNAME"),"/Documents/R/win-library/4.0")) # wouldn't need if added to renv.

library(RDCOMClient)

# Convert the Excel file to ods -------------------------------------------
convert_to_ods(xlsx_file_region)

convert_to_ods(xlsx_file_national)

# move ods file to E&S fileshare
# archive existing file

old_filename <- paste0(Sys.getenv("E_AND_S_FILESHARE"),
                       "RegionalEMData",
                       date_as_string(path_region, 
                                      week_type = "first friday last month",
                                      date_type = "publication date"),
                       # live_or_test_suffix,
                       ".ods")

current_filename <- paste0(Sys.getenv("E_AND_S_FILESHARE"),
                           "RegionalEMData", 
                           live_or_test_suffix,
                           ".ods")
# if last week's file hasn't been archived already with a date stamp, then do it
if (!file.exists(old_filename))
  file.copy(from = current_filename,
            to = old_filename)

# then copy the version created from the "convert_to_ods()" above over the 
# previous "current" version for putting on the website
file.copy(from = paste0(Sys.getenv("POWERBI_FILESHARE"),
                        "/RegionalEMData", 
                        live_or_test_suffix,
                        ".ods"),
          to = current_filename,
          overwrite = TRUE)

old_filename <- paste0(Sys.getenv("E_AND_S_FILESHARE"),
                       "EMData",
                       date_as_string(path_national, 
                                      week_type = "last week",
                                      date_type = "publication date"),
                       live_or_test_suffix,
                       ".ods")

current_filename <- paste0(Sys.getenv("E_AND_S_FILESHARE"),
                           "EMData", 
                           live_or_test_suffix,
                           ".ods")
# if last week's file hasn't been archived already with a date stamp, then do it
if (!file.exists(old_filename))
  file.copy(from = current_filename,
            to = old_filename)

# then copy the version created from the "convert_to_ods()" above over the 
# previous "current" version for putting on the website
file.copy(from = paste0(Sys.getenv("POWERBI_FILESHARE"),
                        "/EMData.ods"),
          to = current_filename,
          overwrite = TRUE)

# Compare the regional registered, covid and excess deaths with what the ONS
# have published
ons_vs_ohid <- compare_regional_to_ons()
View(ons_vs_ohid)