## GET utility functions
source("R/libraries.R")
source("R/utils.R")
source("R/function_deaths_data.R")
source("R/function_monthly_populations.R")
source("R/function_modelling.R")
source("tests/test-assertr.R")
# memory.limit(32698)

eth_dep_denominators <- get_denominators(start_year = 2015,
                                         end_year = 2019,
                                         eth_dep = TRUE)
# debug(create_baseline)
eth_dep_model <- create_baseline(model_filename = "20210319_eth_dep",
                                 denominators = eth_dep_denominators,
                                 eth_dep = TRUE,
                                 include_ethnicity_uplift = TRUE)


denominators <- get_denominators(start_year = 2015,
                                 end_year = 2019,
                                 eth_dep = FALSE)

utla_model <- create_baseline(model_filename = "20210319_utla", 
                              denominators = denominators)


### CAUSE OF DEATH MODELS
# Ischemic heart disease
ihd_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "ischaemic heart diseases") %>% 
  pull(ICD_codes)
ihd_model <- create_baseline(model_filename = "20210113_ihd", 
                             ucods = ihd_codes,
                             denominators = denominators,
                             disease_name = "IHD")

# Stroke
cereberovascular_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "cerebrovascular diseases") %>% 
  pull(ICD_codes)
cerebrovascular_model <- create_baseline(model_filename = "20210113_cerebrovascular", 
                                         ucods = cereberovascular_codes,
                                         denominators = denominators,
                                         disease_name = "Stroke")

# Acute respiratory infections
ari_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "acute respiratory infections") %>% 
  pull(ICD_codes)
ari_model <- create_baseline(model_filename = "20210113_ari", 
                             ucods = ari_codes,
                             denominators = denominators,
                             disease_name = "Acute resp infection")

# Chronic lower respiratory diseases
clr_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "chronic lower respiratory diseases") %>% 
  pull(ICD_codes)
clr_model <- create_baseline(model_filename = "20210113_clr", 
                             ucods = clr_codes, 
                             denominators = denominators,
                             disease_name = "CLRD")

# Other respiratory
other_respiratory_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "other respiratory diseases") %>% 
  pull(ICD_codes)
other_respiratory_model <- create_baseline(model_filename = "20210113_other_respiratory", 
                                           ucods = other_respiratory_codes, 
                                           denominators = denominators,
                                           disease_name = "Other respiratory")

urinary_system_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "diseases of the urinary system") %>% 
  pull(ICD_codes)
urinary_system_model <- create_baseline(model_filename = "20210113_urinary_system",
                                  ucods = urinary_system_codes,
                                  denominators = denominators,
                                  disease_name = "Urinary")

cirrhosis_liver_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "cirrhosis and other liver diseases") %>% 
  pull(ICD_codes)
cirrhosis_liver_model <- create_baseline(model_filename = "20210113_cirrhosis_liver",
                                  ucods = cirrhosis_liver_codes,
                                  denominators = denominators,
                                  disease_name = "Cirrhosis")

parkinsons_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "Parkinson's disease") %>% 
  pull(ICD_codes)
parkinsons_model <- create_baseline(model_filename = "20210113_parkinsons",
                                    ucods = parkinsons_codes,
                                    denominators = denominators,
                                    disease_name = "Parkinsons")

cancer_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "cancer") %>% 
  pull(ICD_codes)
cancer_model <- create_baseline(model_filename = "20210113_cancer",
                                ucods = cancer_codes,
                                denominators = denominators,
                                disease_name = "Cancer")

dementia_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "dementia and Alzheimer's") %>% 
  pull(ICD_codes)
dementia_model <- create_baseline(model_filename = "20210113_dementia",
                                  ucods = dementia_codes,
                                  denominators = denominators,
                                  disease_name = "Dementia/Alzheimers")

other_circulatory_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "other circulatory diseases") %>% 
  pull(ICD_codes)
other_circulatory_diseases_model <- create_baseline(model_filename = "20210113_other_circulatory",
                                                    ucods = other_circulatory_codes,
                                                    denominators = denominators,
                                                    disease_name = "Other circulatory")

all_other_codes <- cause_code_lookup(table_output = TRUE) %>% 
  filter(name_of_cause == "All other causes (excl. COVID-19)") %>% 
  pull(ICD_codes)
all_other_causes_model <- create_baseline(model_filename = "20210113_all_other_causes",
                                          ucods = all_other_codes,
                                          denominators = denominators,
                                          disease_name = "Other (whole list)")

dementia_mentions_model <- create_baseline(model_filename = "20210113_dementia_mentions",
                                           cod = dementia_codes,
                                           denominators = denominators)

ari_mentions_model <- create_baseline(model_filename = "20210113_ari_mentions", 
                                      cod = paste0("J", formatC(0:22, width = 2, flag = "0")),
                                      denominators = denominators)

dm_model <- create_baseline(model_filename = "20210113_diabetes_mentions", 
                            cod = paste0("E", 10:14),
                            denominators = denominators)


#### MORTALITY BY PLACE OF DEATH ####

home_model <- create_baseline(model_filename = "20210113_home", 
                              pod = "home", 
                              denominators = denominators)

care_home_model <- create_baseline(model_filename = "20210113_care_home", 
                                   pod = "care home", 
                                   denominators = denominators)


hospital_model <- create_baseline(model_filename = "20210113_hospital", 
                                  pod = "hospital", 
                                  denominators = denominators)


hospice_model <- create_baseline(model_filename = "20210113_hospice", 
                                 pod = "hospice", 
                                 denominators = denominators)


other_places_model <- create_baseline(model_filename = "20210113_other_places", 
                                      pod = "other", 
                                      denominators = denominators)


