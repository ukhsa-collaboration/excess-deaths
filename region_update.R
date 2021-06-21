source("R/libraries.R")
source("R/function_predictions.R")
source("R/function_visualisations.R")
source("R/utils.R")
source("R/function_monthly_populations.R")
source("tests/test-assertr.R")
source("R/function_deaths_data.R")
source("R/utils_charts.R")
source("R/utils_phecharts.R")
source("R/collate_nat_exmort.R")
final_date <- final_report_date()
# final_date <- as.Date("2020-03-27") + 14

caption <- paste("\nLatest week not available as ethnicity has to be assigned via linkage with hospital records")
ethnic_group_final_date <- final_date - 7

eth_dep_setting <- TRUE
age_group_type_setting <- "nomis"
pop_type_setting <- "estimates"
from_date <- as.Date("2020-03-21")

rgn_charts <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                      visualisation_geography = "region",
                                      include_covid_ribbon = TRUE, 
                                      end_date = final_date, 
                                      from_date = from_date,
                                      eth_dep = eth_dep_setting, 
                                      show_inset = TRUE, 
                                      age_group_type = age_group_type_setting)

rgn_as_charts <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                         visualisation_geography = "region",
                                         facet_fields = c("Age_Group", "Sex"),
                                         split_chart = "Sex",
                                         include_covid_ribbon = TRUE, 
                                         eth_dep = eth_dep_setting, 
                                         end_date = final_date, 
                                         from_date = from_date,
                                         include_totals = TRUE,
                                         age_group_type = age_group_type_setting, 
                                         axis_title = "Age group (years)")

utla_charts <- generate_visualisations(paste0("model_outputs/", all_models("UTLA"), ".rds"),
                                       visualisation_geography = "region", 
                                       facet_fields = "UTLAApr19CD",
                                       split_chart = "UTLAApr19CD",
                                       end_date = final_date,
                                       from_date = from_date,
                                       include_covid_ribbon = TRUE, 
                                       axis_title = "Upper Tier Local Authority")

#### ETHNICITY CHARTS ####
rgn_ethnicity_sex <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                             visualisation_geography = "region",
                                             facet_fields = c("Ethnic_Group", "Sex"), 
                                             split_chart = "Sex",
                                             end_date = ethnic_group_final_date, 
                                             from_date = from_date,
                                             include_covid_ribbon = TRUE, 
                                             eth_dep = eth_dep_setting, 
                                             age_group_type = age_group_type_setting, 
                                             axis_title = "Ethnic group",
                                             caption = caption)
#### DEPRIVATION CHARTS ####
rgn_deprivation <- generate_visualisations(paste0("model_outputs/", all_models("Ethnicity-deprivation"), ".rds"),
                                           visualisation_geography = "region",
                                           facet_fields = "Deprivation_Quintile", 
                                           split_chart = "Deprivation_Quintile",
                                           end_date = final_date, 
                                           from_date = from_date,
                                           include_covid_ribbon = TRUE, 
                                           eth_dep = eth_dep_setting, 
                                           age_group_type = age_group_type_setting,
                                           axis_title = "Deprivation quintile")


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
                                       cause_name = "",
                                       caption = "",
                                       include_covid_ribbon = FALSE,
                                       end_date = final_date,
                                       from_date = from_date,
                                       visualisation_geography = "region",
                                       stop_before_visualisation = TRUE))

files <- model_files %>%
  mutate(model_file = basename(model_file),
         model_file = xfun::sans_ext(model_file),
         model_file = paste0(
           Sys.getenv("POWERBI_FILESHARE"), "/",
           model_file,
           "_RGN09CD_ons_aligned_weekly.csv"
         )
  ) %>%
  select(-codes)

regions <- utla_lookup() %>%
  distinct(RGN09CD, RGN09NM)

cod_plot_data <- pmap(
  files,
  function(model_file, reference){
    read.csv(model_file) %>%
      mutate(name_of_cause = reference)
  }
) %>%
  bind_rows() %>%
  filter(name_of_cause != "Epilepsy") %>%
  mutate(
    name_of_cause = factor(name_of_cause, levels = c("Ischaemic heart diseases",
                                                     "Cerebrovascular diseases",
                                                     "Other circulatory diseases",
                                                     "Heart failure",
                                                     "Cancer",
                                                     "Acute respiratory infections",
                                                     "Chronic lower respiratory diseases",
                                                     "Other respiratory diseases",
                                                     "Dementia and Alzheimer's",
                                                     "Diseases of the urinary system",
                                                     "Cirrhosis and other liver diseases",
                                                     "Diabetes",
                                                     "Parkinson's disease"
                                                   )
                    )
  ) %>%
  left_join(regions,
            by = "RGN09CD") %>%
  split(.[["RGN09NM"]])


rgn_ucods <- map(
  cod_plot_data,
  function(.x){
    cumulative_compare(
      death_data = .x,
      area_name = unique(.x[["RGN09NM"]]),
      date_field = date, 
      model_field = expected, 
      deaths_field = registered,
      covid_field = ucod_covid,
      axis_field = "name_of_cause",
      start_date = from_date,
      subtitle = "",
      end_date = final_date,
      axis_title = "Cause of death mentioned on death certificate",
      caption = "",
      dispersion_parameter = 1,
      include_covid_ribbon = FALSE,
      breakdown_by_ucod = TRUE
    )
  }
)


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
                                        include_covid_ribbon = TRUE,
                                        axis_title = "Place of death")

mysubtitle <- "Experimental Statistics"

icd_references <- model_references() %>% 
  filter(!is.na(caption),
         !is.na(reference)) %>% 
  mutate(caption = gsub("ICD10 reference: ", "", caption),
         reference = capitalise_first_letter(reference)) %>% 
  dplyr::select(`Cause description` = reference,
                `ICD10 reference` = caption) %>% 
  map_df(rev)

for (reg in names(rgn_charts$weekly_simple)) {
  reg_file <- gsub(" ", "-", reg)
  input_params <- list(mytitle = paste("Excess mortality in ", reg, "- 21 March 2020 to", format(final_date, "%d %B %Y")),
                       mysubtitle = mysubtitle,
                       author = "[What is an experimental statistic?](https://www.ons.gov.uk/methodology/methodologytopicsandstatisticalconcepts/guidetoexperimentalstatistics)",
                       rgn_name = reg,
                       rgn_charts = rgn_charts,
                       rgn_as_charts = rgn_as_charts,
                       utla_charts = utla_charts,
                       rgn_ethnicity_sex = rgn_ethnicity_sex,
                       rgn_deprivation = rgn_deprivation,
                       rgn_ucods = rgn_ucods,
                       rgn_all_pods = rgn_all_pods,
                       icd_references = icd_references,
                       period_setting = "weekly")
  
  
  system.time(rmarkdown::render("report/region_report.Rmd",
                                params = input_params,
                                output_file = paste0("excess-mortality-in-", reg_file, "-21-March-2020-to-", format(final_date, "%d-%B-%Y"), ".html")))
}

source("tests/test-regional_report_totals.R")
# This checks the totals of all the tables in each report with the cumulative chart for each report
checks <- perform_regional_checks(region_charts = rgn_charts,
                                  age_sex_charts = rgn_as_charts,
                                  ethnicity_sex_charts = rgn_ethnicity_sex,
                                  deprivation_charts = rgn_deprivation,
                                  all_utla_charts = utla_charts,
                                  ucod_charts = rgn_ucods,
                                  pod_charts = rgn_all_pods)
View(checks)


# this checks the totals for the regional reports for the mentions charts 
# against the equivalent totals in the national report
path_region <- collate_powerbi_files_for_powerbi(geography = "region", 
                                                 final_date = final_date)

path_national <- collate_powerbi_files_for_powerbi(geography = "england", 
                                                   final_date = final_date)


compare_mentions_region_national <- list(region = path_region,
                                         national = path_national) %>% 
  lapply(function(x) x %>% 
           read.csv() %>% 
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
  filter(region_minus_national != 0) %>% 
  dplyr::select(!c(val.x, val.y)) %>% 
  arrange(desc(abs(region_minus_national)))

View(compare_mentions_region_national)
