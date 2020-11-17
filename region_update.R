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
#publication <- "circulation"

caption <- paste("\nLatest week not available as ethnicity has to be assigned via linkage with hospital records")
ethnic_group_final_date <- final_date - 7

rgn_charts <- generate_visualisations("model_outputs/glm_all_utlas_20200522.rds",
                                      visualisation_geography = "region",
                                      include_covid_ribbon = TRUE, 
                                      end_date = final_date, 
                                      deprivation = FALSE, 
                                      show_inset = TRUE)

rgn_as_charts <- generate_visualisations("model_outputs/glm_all_utlas_20200522.rds",
                                         visualisation_geography = "region",
                                         facet_fields = c("Age_Group", "Sex"),
                                         split_chart = "Sex",
                                         include_covid_ribbon = TRUE, 
                                         end_date = final_date, 
                                         deprivation = FALSE, 
                                         axis_title = "Age group (years)")

utla_charts <- generate_visualisations("model_outputs/glm_all_utlas_20200522.rds",
                                       visualisation_geography = "region", 
                                       facet_fields = "UTLAApr19CD",
                                       split_chart = "UTLAApr19CD",
                                       end_date = final_date,
                                       include_covid_ribbon = TRUE, 
                                       deprivation = FALSE,
                                       axis_title = "Upper Tier Local Authority")

#### ETHNICITY CHARTS ####
rgn_ethnicity_sex <- generate_visualisations("model_outputs/20200619_ethnicity.rds",
                                             ethnicity = TRUE, 
                                             visualisation_geography = "region",
                                             facet_fields = c("Ethnic_Group", "Sex"), 
                                             split_chart = "Sex",
                                             end_date = ethnic_group_final_date, 
                                             include_covid_ribbon = TRUE, 
                                             axis_title = "Ethnic group",
                                             caption = caption)
#### DEPRIVATION CHARTS ####
rgn_deprivation <- generate_visualisations("model_outputs/20200616_deprivation.rds",
                                           deprivation = TRUE, 
                                           visualisation_geography = "region",
                                           facet_fields = "Deprivation_Quintile", 
                                           end_date = final_date, 
                                           include_covid_ribbon = TRUE,
                                           axis_title = "Deprivation quintile")


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

rgn_all_ucods <- generate_visualisations(model_filename = model_files,
                                         all_ucod = TRUE,
                                         end_date = final_date, 
                                         deprivation = FALSE,
                                         visualisation_geography = "region",
                                         facet_fields = "name_of_cause",
                                         split_chart = "name_of_cause",
                                         include_covid_ribbon = TRUE,
                                         caption = captions,
                                         axis_title = "Underlying cause of death")

# dementia mentions
rgn_dementia_mentions_vis <- generate_visualisations(cod = c("F01", "F03", "G30"),
                                                     model_filename = "model_outputs/glm_all_utlas_dementia_mentions_20200522.rds",
                                                     cause_name = "dementia",
                                                     caption = "ICD10 reference: All mentions of F01, F03 and G30",
                                                     include_covid_ribbon = FALSE, 
                                                     end_date = final_date,
                                                     visualisation_geography = "region")

# acute respiratory mentions
rgn_ari_mentions_vis <- generate_visualisations(cod = paste0("J", formatC(0:22, width = 2, flag = "0")),
                                                model_filename = "model_outputs/glm_all_utlas_ari_mentions_20200522.rds",
                                                cause_name = "acute respiratory infections",
                                                caption = "ICD10 reference: All mentions of J00-J22",
                                                include_covid_ribbon = FALSE, 
                                                end_date = final_date,
                                                visualisation_geography = "region")

# diabetes mellitus mentions
rgn_dm_vis <- generate_visualisations(cod = paste0("E", 10:14),
                                      model_filename = "model_outputs/glm_all_utlas_dm_20200522.rds",
                                      cause_name = "diabetes mellitus",
                                      caption = "ICD10 reference: All mentions of E10-E14",
                                      include_covid_ribbon = FALSE, 
                                      end_date = final_date,
                                      visualisation_geography = "region")


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
                                        deprivation = FALSE,
                                        visualisation_geography = "region",
                                        facet_fields = "POD_out",
                                        split_chart = "POD_out",
                                        include_covid_ribbon = TRUE,
                                        axis_title = "Place of death")

if (publication == "external") {
  mysubtitle <- "Experimental Statistics"
} else if (publication == "circulation") {
  mysubtitle <- "OFFICIAL SENSITIVE: Not for wider circulation (Experimental Statistics)"
}

for (reg in names(rgn_charts$weekly_simple)) {
  reg_file <- sub(" ", "-", reg)
  input_params <- list(mytitle = paste("Excess mortality in ", reg, "- 20 March 2020 to ", format(final_date, "%d %B %Y")),
                       mysubtitle = mysubtitle,
                       author = "[What is an experimental statistic?](https://www.ons.gov.uk/methodology/methodologytopicsandstatisticalconcepts/guidetoexperimentalstatistics)",
                       stakeholder = "external",
                       rgn_name = reg,
                       rgn_charts = rgn_charts,
                       rgn_as_charts = rgn_as_charts,
                       utla_charts = utla_charts,
                       rgn_ethnicity_sex = rgn_ethnicity_sex,
                       rgn_deprivation = rgn_deprivation,
                       rgn_all_ucods = rgn_all_ucods,
                       rgn_dementia_mentions_vis = rgn_dementia_mentions_vis,
                       rgn_ari_mentions_vis = rgn_ari_mentions_vis,
                       rgn_dm_vis = rgn_dm_vis,
                       rgn_all_pods = rgn_all_pods)
  
  
  system.time(rmarkdown::render("report/region_report.Rmd",
                                params = input_params,
                                output_file = paste0("excess-mortality-in-", reg_file, "-20-March-2020-to-", format(final_date, "%d-%B-%Y"), ".html")))
}

