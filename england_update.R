source("R/libraries.R")
source("R/function_predictions.R")
source("R/function_visualisations.R")
source("R/utils.R")
source("R/function_monthly_populations.R")
source("tests/test-assertr.R")
source("R/function_deaths_data.R")
source("R/utils_charts.R")
source("R/utils_phecharts.R")
final_date <- final_report_date()
publication <- "external"
# publication <- "circulation"

caption <- paste("\nLatest week not available as ethnicity has to be assigned via linkage with hospital records")
ethnic_group_final_date <- final_date - 7

eng_charts <- generate_visualisations("model_outputs/glm_all_utlas_20200522.rds",
                                      visualisation_geography = "england",
                                      include_covid_ribbon = TRUE, end_date = final_date, 
                                      deprivation = FALSE, show_inset = TRUE)

eng_as_charts <- generate_visualisations("model_outputs/glm_all_utlas_20200522.rds",
                                         visualisation_geography = "england",
                                         facet_fields = c("Age_Group", "Sex"),
                                         split_chart = "Sex",
                                         include_covid_ribbon = TRUE, 
                                         end_date = final_date, 
                                         deprivation = FALSE, 
                                         axis_title = "Age group (years)",
                                         include_totals = TRUE)

region_charts <- generate_visualisations("model_outputs/glm_all_utlas_20200522.rds",
                                         visualisation_geography = "england", 
                                         facet_fields = "RGN09CD",
                                         split_chart = "RGN09CD",
                                         end_date = final_date,
                                         include_covid_ribbon = TRUE, 
                                         deprivation = FALSE,
                                         axis_title = "Region")

#### ETHNICITY CHARTS ####
england_ethnicity_sex <- generate_visualisations("model_outputs/20200619_ethnicity.rds",
                                                 ethnicity = TRUE, 
                                                 visualisation_geography = "england",
                                                 facet_fields = c("Ethnic_Group", "Sex"), 
                                                 split_chart = "Sex",
                                                 end_date = ethnic_group_final_date, 
                                                 include_covid_ribbon = TRUE, 
                                                 axis_title = "Ethnic group",
                                                 caption = caption)
#### DEPRIVATION CHARTS ####
england_deprivation <- generate_visualisations("model_outputs/20200616_deprivation.rds",
                                               deprivation = TRUE, 
                                               visualisation_geography = "england",
                                               facet_fields = "Deprivation_Quintile", 
                                               end_date = final_date, 
                                               include_covid_ribbon = TRUE, 
                                               axis_title  = "Deprivation quintile")


#### MORTALITY BY CAUSE OF DEATH ####

# all diseases compared
causes <- cause_code_lookup(table_output = TRUE) %>%
  filter(name_of_cause != "COVID-19") %>% 
  pull(name_of_cause) %>%
  unique()
model_files <- model_references() %>%
  filter(reference %in% causes) %>%
  dplyr::select(reference, model_file) %>% 
  tibble::deframe()

captions <- model_references() %>%
  filter(reference %in% causes) %>%
  dplyr::select(reference, caption) %>% 
  tibble::deframe()

all_ucods <- generate_visualisations(model_filename = model_files,
                                     all_ucod = TRUE,
                                     end_date = final_date, 
                                     deprivation = FALSE,
                                     visualisation_geography = "england",
                                     facet_fields = "name_of_cause",
                                     split_chart = "name_of_cause",
                                     include_covid_ribbon = TRUE,
                                     caption = captions,
                                     axis_title = "Underlying cause of death")

# dementia mentions
dementia_mentions_vis <- generate_visualisations(cod = c("F01", "F03", "G30"),
                                                 model_filename = "model_outputs/glm_all_utlas_dementia_mentions_20200522.rds",
                                                 cause_name = "all mentions of dementia and Alzheimer's disease",
                                                 caption = "ICD10 reference: All mentions of F01, F03 and G30",
                                                 include_covid_ribbon = FALSE, 
                                                 end_date = final_date,
                                                 visualisation_geography = "england")

# acute respiratory mentions
ari_mentions_vis <- generate_visualisations(cod = paste0("J", formatC(0:22, width = 2, flag = "0")),
                                            model_filename = "model_outputs/glm_all_utlas_ari_mentions_20200522.rds",
                                            cause_name = "all mentions of acute respiratory infections (incl. flu and pneumonia)",
                                            caption = "ICD10 reference: All mentions of J00-J22",
                                            include_covid_ribbon = FALSE, 
                                            end_date = final_date,
                                            visualisation_geography = "england")

# diabetes mellitus mentions
dm_vis <- generate_visualisations(cod = paste0("E", 10:14),
                                  model_filename = "model_outputs/glm_all_utlas_dm_20200522.rds",
                                  cause_name = "all mentions of diabetes mellitus",
                                  caption = "ICD10 reference: All mentions of E10-E14",
                                  include_covid_ribbon = FALSE, 
                                  end_date = final_date,
                                  visualisation_geography = "england")


#### MORTALITY BY PLACE OF DEATH ####
# All places of death combined
pods <- pod_lookup() %>%
  pull(pod_filter) %>%
  unique()
model_files <- model_references() %>%
  filter(reference %in% pods) %>%
  dplyr::select(reference, model_file) %>% 
  tibble::deframe()

all_pods <- generate_visualisations(model_filename = model_files,
                                    all_pod = TRUE,
                                    end_date = final_date, 
                                    deprivation = FALSE,
                                    visualisation_geography = "england",
                                    facet_fields = "POD_out",
                                    split_chart = "POD_out",
                                    include_covid_ribbon = TRUE,
                                    axis_title = "Place of death")

if (publication == "external") {
  mysubtitle <- "Experimental Statistics"
} else if (publication == "circulation") {
  mysubtitle <- "OFFICIAL SENSITIVE: DRAFT - Not for wider circulation (Experimental Statistics)"
}

input_params <- list(mytitle = paste("Excess mortality in England, week ending", format(final_date, "%d %B %Y")),
                     mysubtitle = mysubtitle,
                     author = "[What is an experimental statistic?](https://www.ons.gov.uk/methodology/methodologytopicsandstatisticalconcepts/guidetoexperimentalstatistics)",
                     eng_charts = eng_charts,
                     eng_as_charts = eng_as_charts,
                     region_charts = region_charts,
                     utla_lkp = utla_lookup(),
                     england_ethnicity_sex = england_ethnicity_sex,
                     england_deprivation = england_deprivation,
                     all_ucods = all_ucods,
                     dementia_mentions_vis = dementia_mentions_vis,
                     ari_mentions_vis = ari_mentions_vis,
                     dm_vis = dm_vis,
                     all_pods = all_pods)

output_filename <- paste0("excess-mortality-in-england-week-ending-",
                          format(final_date, "%d-%b-%Y"), ".html")

system.time(rmarkdown::render("report/gov_uk.Rmd",
                              params = input_params,
                              output_file = output_filename))

