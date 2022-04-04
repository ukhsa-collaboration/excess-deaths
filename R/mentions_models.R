source("R/libraries.R")
source("R/function_predictions.R")
source("R/function_visualisations.R")
source("R/utils.R")
source("R/function_monthly_populations.R")
source("tests/test-assertr.R")
source("R/function_deaths_data.R")
source("R/collate_nat_exmort.R")
source("R/function_modelling.R")
source("tests/test-assertr.R")

causes_of_death <- cause_code_lookup(table_output = TRUE)

# date of my newly run models with mentions and underlying causes
date <- "20220211"

# all our cause names and a fileame for the models
names <- causes_of_death %>%
  # can't make a model for covid and all other causes is too big - breaks SQL!
  filter(!name_of_cause %in% c("COVID-19", "All other causes (excl. COVID-19)")) %>%
  group_by(name_of_cause) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    # take out underscores
    file_name = gsub(" ", "_", name_of_cause),
    # take out everything but letters and underscores
    file_name = gsub("[^[:alpha:]|_]", "", file_name),
    file_name = str_to_lower(file_name),
    file_name = paste0(date, "_", file_name, "_mentions")
  ) %>%
  # arranged by count for when I was testing to see which query was too big,
  # not really necessary
  arrange(count) %>%
  dplyr::select(-count)

denominators <- get_denominators(start_year = 2015,
                                 end_year = 2019,
                                 eth_dep = FALSE)

# use our names table to create the models
make_mention_models <- function(name_of_cause, file_name, causes_of_death, denominators){
  
  codes <- causes_of_death %>%
    filter(name_of_cause == {{name_of_cause}}) %>%
    pull(ICD_codes)
  
  model <- create_baseline(
    model_filename = file_name,
    cod = codes,
    denominators = denominators
  )
  
  file_name
  
}

# walk through the names dataframe to make models
pwalk(.l = names, .f = make_mention_models,
      causes_of_death = causes_of_death,
      denominators = denominators
)



# create cancer models ----------------------------------------------------
cancers <- cause_code_lookup(table_output = TRUE, 
                             detail_cancers = TRUE) %>% 
  filter(grepl("^C", ICD_codes))

date <- "20220110"

cancer_names <- cancers %>%
  filter(!name_of_cause %in% c("COVID-19", "All other causes (excl. COVID-19)")) %>%
  count(name_of_cause) %>%
  mutate(
    # take out underscores
    file_name = gsub(" ", "_", name_of_cause),
    # take out everything but letters and underscores
    file_name = gsub("[^[:alpha:]|_]", "", file_name),
    file_name = str_to_lower(file_name),
    file_name = paste0(date, "_", file_name, "_mentions")
  ) %>% 
  dplyr::select(!c(n))

# walk through the names dataframe to make models
pwalk(.l = cancer_names[cancer_names$name_of_cause == "colon_sigmoid_rectum_anus_cancer", ],
      .f = make_mention_models,
      causes_of_death = cancers,
      denominators = denominators
)

# stolen form england_update.R -------------------------------------------------
final_date <- final_report_date()
from_date <- as.Date("2020-03-21")


# make the visualisations from our models
make_visualisations <- function(name_of_cause, file_name, causes_of_death){
  
  codes <- causes_of_death %>%
    filter(name_of_cause == {{name_of_cause}}) %>%
    pull(ICD_codes)
  
  generate_visualisations(
    ucods = codes,
    cod = codes,
    model_filename = paste0("model_outputs/", file_name, ".rds"),
    cause_name = paste0("all mentions of ", name_of_cause),
    end_date = final_date,
    from_date = from_date,
    visualisation_geography = "region"
  )
  
}

# make our visualisations
pwalk(.l = cancer_names[cancer_names$name_of_cause == "colon_sigmoid_rectum_anus_cancer", ], 
      .f = make_visualisations,
      causes_of_death = cancers
)

# Heart failure
date <- "20220211"

name_of_cause <- "Heart failure"
file_name <- paste0(date, "_heart_failure_mentions")

heart_failure_codes <- tibble(
  name_of_cause = "Heart failure",
  ICD_codes = c("I110", "I255", "I420", "I429", "I500", "I501", "I509")
)

# walk through the names dataframe to make models
model <- make_mention_models(name_of_cause, file_name, heart_failure_codes, denominators)
plots <- make_visualisations(name_of_cause, file_name, heart_failure_codes)



