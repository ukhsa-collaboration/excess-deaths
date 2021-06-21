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
source("R/function_modelling.R")
source("tests/test-assertr.R")

causes_of_death <- cause_code_lookup(table_output = TRUE)

# date of my newly run models with mentions and underlying causes
date <- "20210302"

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
  select(-count)

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


# stolen form england_update.R -------------------------------------------------
final_date <- final_report_date()

caption <- paste("\nLatest week not available as ethnicity has to be assigned via linkage with hospital records")
ethnic_group_final_date <- final_date - 7


eth_dep_setting <- TRUE
age_group_type_setting <- "nomis" 
pop_type_setting <- "estimates"
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
    include_covid_ribbon = FALSE,
    end_date = final_date,
    from_date = from_date,
    visualisation_geography = "england"
  )

}

# make our visualisations
pwalk(.l = names, .f = make_visualisations,
      causes_of_death = causes_of_death
)

# Heart failure
date <- "20210302"

name_of_cause <- "Heart failure"
file_name <- paste0(date, "_heart_failure_mentions")

heart_failure_codes <- tibble(
  name_of_cause = "Heart failure",
  ICD_codes = c("I110", "I255", "I420", "I429", "I500", "I501", "I509")
)

# walk through the names dataframe to make models
model <- make_mention_models(name_of_cause, file_name, heart_failure_codes, denominators)
plots <- make_visualisations(name_of_cause, file_name, heart_failure_codes)


# Epilepsy
date <- "20210309"

name_of_cause <- "Epilepsy"
file_name <- paste0(date, "_epilepsy_mentions")

epilepsy_codes <- tibble(
  name_of_cause = name_of_cause,
  ICD_codes = c(paste0("G40", 0:9), "G410", "G411", "G412", "G418", "G419", "R568")
)

# walk through the names dataframe to make models
model <- make_mention_models(name_of_cause, file_name, epilepsy_codes, denominators)
plots <- make_visualisations(name_of_cause, file_name, epilepsy_codes)
