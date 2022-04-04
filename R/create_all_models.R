## GET utility functions
source("R/libraries.R")
source("R/utils.R")
source("R/function_deaths_data.R")
source("R/function_monthly_populations.R")
source("R/function_modelling.R")
source("tests/test-assertr.R")
memory.limit(32698)

eth_dep_denominators <- get_denominators(start_year = 2015,
                                         end_year = 2019,
                                         eth_dep = TRUE)
# debug(create_baseline)
eth_dep_model <- create_baseline(model_filename = "20220210_eth_dep",
                                 denominators = eth_dep_denominators,
                                 eth_dep = TRUE)


denominators <- get_denominators(start_year = 2015,
                                 end_year = 2019,
                                 eth_dep = FALSE)

utla_model <- create_baseline(model_filename = "20220211_utla", 
                              denominators = denominators)


## Cause of death models are now in R/mentions_models.R

#### MORTALITY BY PLACE OF DEATH ####

home_model <- create_baseline(model_filename = "20220211_home", 
                              pod = "home", 
                              denominators = denominators)

care_home_model <- create_baseline(model_filename = "20220211_care_home", 
                                   pod = "care home", 
                                   denominators = denominators)


hospital_model <- create_baseline(model_filename = "20220211_hospital", 
                                  pod = "hospital", 
                                  denominators = denominators)


hospice_model <- create_baseline(model_filename = "20220211_hospice", 
                                 pod = "hospice", 
                                 denominators = denominators)


other_places_model <- create_baseline(model_filename = "20220211_other_places", 
                                      pod = "other", 
                                      denominators = denominators)


