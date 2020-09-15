## GET utility functions
source("R/utils.R")
source("R/function_deaths_data.R")
source("R/function_monthly_populations.R")
source("R/function_modelling.R")
source("tests/test-assertr.R")
# memory.limit(32698)

ethnicity_denominators <- get_denominators(start_year = 2015,
                                           end_year = 2019,
                                           ethnicity = TRUE)

ethnicity_model <- create_baseline(model_filename = "20200619_ethnicity",
                                   denominators = ethnicity_denominators,
                                   ethnicity = TRUE,
                                   include_2019 = TRUE,
                                   include_ethnicity_uplift = TRUE)


dep_denominators <- get_denominators(start_year = 2015,
                                     end_year = 2019,
                                     ethnicity = FALSE,
                                     deprivation = TRUE)

reg_deprivation_model <- create_baseline(model_filename = "20200616_deprivation", 
                                         denominators = dep_denominators, 
                                         deprivation = TRUE,
                                         include_2019 = TRUE)

denominators <- get_denominators(start_year = 2015,
                                 end_year = 2019,
                                 ethnicity = FALSE,
                                 deprivation = FALSE)
debug(create_baseline)                                 
utla_model <- create_baseline(model_filename = "glm_all_utlas_20200608", 
                              denominators = denominators,
                              include_2019 = TRUE)

### CAUSE OF DEATH MODELS
# Ischemic heart disease
ihd_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "ischaemic heart diseases") %>% 
  pull(ICD_codes)
ihd_model <- create_baseline(model_filename = "20200626_ihd", 
                             ucods = ihd_codes,
                             denominators = denominators,
                             include_2019 = TRUE,
                             disease_name = "IHD")

# Stroke
cereberovascular_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "cerebrovascular diseases") %>% 
  pull(ICD_codes)
cerebrovascular_model <- create_baseline(model_filename = "20200626_cerebrovascular", 
                                         ucods = cereberovascular_codes,
                                         denominators = denominators,
                                         include_2019 = TRUE,
                                         disease_name = "Stroke")

# # Other circulatory
# other_circulatory_codes <- cause_code_lookup(table_output = TRUE) %>% 
#   filter(name_of_cause == "other circulatory diseases") %>% 
#   pull(ICD_codes)
# other_circulatory_model <- create_baseline(model_filename = "20200626_other_circulatory", 
#                                            ucods  = other_circulatory_codes, 
#                                            denominators = denominators,
#                                            include_2019 = TRUE,
#                                            disease_name = "Other circulatory")
# 
# # Cancer
# cancer_codes <- cause_code_lookup(table_output = TRUE) %>% 
#   filter(name_of_cause == "cancer") %>% 
#   pull(ICD_codes)
# cancer_model <- create_baseline(model_filename = "20200626_cancer", 
#                                 ucods = cancer_codes,
#                                 denominators = denominators,
#                                 include_2019 = TRUE,
#                                 disease_name = "Cancer")

# Acute respiratory infections
ari_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "acute respiratory infections") %>% 
  pull(ICD_codes)
ari_model <- create_baseline(model_filename = "20200626_ari", 
                             ucods = ari_codes,
                             denominators = denominators,
                             include_2019 = TRUE,
                             disease_name = "Acute resp infection")

# Chronic lower respiratory diseases
clr_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "chronic lower respiratory diseases") %>% 
  pull(ICD_codes)
clr_model <- create_baseline(model_filename = "20200626_clr", 
                             ucods = clr_codes, 
                             denominators = denominators,
                             include_2019 = TRUE,
                             disease_name = "CLRD")

# Other respiratory
other_respiratory_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "other respiratory diseases") %>% 
  pull(ICD_codes)
other_respiratory_model <- create_baseline(model_filename = "20200626_other_respiratory", 
                                           ucods = other_respiratory_codes, 
                                           denominators = denominators,
                                           include_2019 = TRUE,
                                           disease_name = "Other respiratory")

# dementia_codes <- cause_code_lookup(table_output = TRUE) %>% 
#   filter(name_of_cause == "dementia and Alzheimer's") %>% 
#   pull(ICD_codes)
# dementia_model <- create_baseline(model_filename = "20200626_dementia",
#                                   ucods = dementia_codes,
#                                   denominators = denominators,
#                                   include_2019 = TRUE,
#                                   disease_name = "Dementia/Alzheimers")

urinary_system_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "diseases of the urinary system") %>% 
  pull(ICD_codes)
urinary_system_model <- create_baseline(model_filename = "20200626_urinary_system",
                                  ucods = urinary_system_codes,
                                  denominators = denominators,
                                  include_2019 = TRUE,
                                  disease_name = "Urinary")

cirrhosis_liver_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "cirrhosis and other liver diseases") %>% 
  pull(ICD_codes)
cirrhosis_liver_model <- create_baseline(model_filename = "20200626_cirrhosis_liver",
                                  ucods = cirrhosis_liver_codes,
                                  denominators = denominators,
                                  include_2019 = TRUE,
                                  disease_name = "Cirrhosis")

parkinsons_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "Parkinson's disease") %>% 
  pull(ICD_codes)
parkinsons_model <- create_baseline(model_filename = "20200626_parkinsons",
                                    ucods = parkinsons_codes,
                                    denominators = denominators,
                                    include_2019 = TRUE,
                                    disease_name = "Parkinsons")

# all_other_codes <- cause_code_lookup(table_output = TRUE) %>% 
#   filter(name_of_cause == "All other causes (excl. COVID-19)") %>% 
#   pull(ICD_codes)
# all_other_model <- create_baseline(model_filename = "20200626_all_other",
#                                    ucods = all_other_codes,
#                                    denominators = denominators,
#                                    include_2019 = TRUE,
#                                    disease_name = "Other (whole list)")

dementia_mentions_model <- create_baseline(model_filename = "glm_all_utlas_dementia_mentions_20200522",
                                           cod = dementia_codes,
                                           denominators = denominators,
                                           include_2019 = TRUE)
ari_mentions_model <- create_baseline(model_filename = "glm_all_utlas_ari_mentions_20200522", 
                                      cod = paste0("J", formatC(0:22, width = 2, flag = "0")),
                                      denominators = denominators,
                                      include_2019 = TRUE)

dm_model <- create_baseline(model_filename = "glm_all_utlas_dm_20200522", 
                            cod = paste0("E", 10:14),
                            denominators = denominators,
                            include_2019 = TRUE)


#### MORTALITY BY PLACE OF DEATH ####

home_model <- create_baseline(model_filename = "glm_all_utlas_home_20200522", 
                              pod = "home", 
                              denominators = denominators,
                              include_2019 = TRUE)

care_home_model <- create_baseline(model_filename = "glm_all_utlas_care_home_20200522", 
                                   pod = "care home", 
                                   denominators = denominators,
                                   include_2019 = TRUE)


hospital_model <- create_baseline(model_filename = "glm_all_utlas_hospital_20200522", 
                                  pod = "hospital", 
                                  denominators = denominators,
                                  include_2019 = TRUE)


hospice_model <- create_baseline(model_filename = "glm_all_utlas_hospice_20200522", 
                                 pod = "hospice", 
                                 denominators = denominators,
                                 include_2019 = TRUE)


other_places_model <- create_baseline(model_filename = "glm_all_utlas_other_places_20200522", 
                                      pod = "other", 
                                      denominators = denominators,
                                      include_2019 = TRUE)


