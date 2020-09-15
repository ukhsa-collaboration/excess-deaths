
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Description

This repository contains the analysis that produces the [weekly excess
deaths
estimates](https://www.gov.uk/government/publications/excess-mortality-in-england-weekly-reports)
published by Public Health England.

This README file contains instructions for how to use the repository
including:

  - [Data requirements](#data-requirements)
  - [Structure of the repository](#structure-of-the-repository)
  - [Recreating package versions using
    `renv`](#recreating-package-versions-using-renv)
  - [Guidance on how to produce the
    reports](#guidance-on-how-to-produce-the-reports)
  - [Contributors](#contributors)

### Data requirements

These reports require:

  - record level data for deaths, which include information on age, sex,
    resident geography, ethnicity, place of death and cause of death.
    These data cover the time period January 2015 to December 2019, and
    then deaths from March 20th 2020 onwards
  - population estimates for the same time periods described above
    copntaining the same demographic information as above

The [methodology
document](https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/900236/EM_Methods1.pdf)
describes the data used in more detail.

### Guidance on how to produce the reports

The reports are built in two stages. Stage 1 creates and saves the model
objects that are based on historic data. These objects are then used in
Stage 2 for predicting expected deaths.

**Stage 1: R/create\_all\_models.R**

This script uses `get_denominators()` from the
`R/function_deaths_data.R` file, and `create_baseline()` from the
`R/function_modelling.R` file. It imports daily deaths data from the
data repository and builds and saves model objects.

**Stage 2: england\_update.R or region\_update.R**

The `england_update.R` file in the root directory uses
`generate_visualisations()` from `R/function_visualisations.R` file.
This function uses the model outputs from Stage 1 to predict daily
deaths, along with prediction intervals, for recent weeks. This stage
only occurs if the predictions for the current month haven’t been
created before. If they haven’t, the predictions get stored in csv files
in a folder called `predictions` in specified location in a shared area.
If they have already been generated, these files are read straight into
the R session. The function then imports death data from the recent
deaths database, which are plotted alongside the predicted deaths. The
output from `generate_visualisations()` is a list of ggplot objects,
which then get passed into `report/gov_uk.Rmd` to produce the html
report.

### Structure of the repository

    +-- R/
    |   +-- function_deaths_data.R - functions to import deaths data from the data repository in different cuts
    |   +-- function_modelling.R - a function to create model objects for each subsection of the population
    |   +-- function_monthly_populations.R - a function to generate monthly populations from the data repository
    |   +-- function_predictions.R - function to apply models generated from function_modelling.R to make predictions for recent dates
    |   +-- function_visualisations.R - function that combines predictions from function_predictions.R with actual deaths data from the daily deaths database, to produce the visualisations for reporting
    |   +-- utils.R - some general utility functions
    |   +-- utils_charts.R - charting functions for the report
    |   +-- utils_phecharts.R - branding functions
    +-- data/ - contains lookup files that aren't available centrally
    +-- tests/ - test-assertr.R; some functions included to ensure data processing within functions is working as expected
    +-- renv/ - automatically generated using the renv package, no need to modify
    +-- report/ 
    |   +-- gov_uk.Rmd- England report
    |   +-- region_report.Rmd - regional report
    |   +-- supporting files for aesthetics for Rmd documents
    +-- england_update.R - the script that generates the report for England
    +-- region_update.R - the script that generates the regional reports

### Recreating package versions using `renv`

Package versioning with this project is being done using the `renv`
package. If this package isn’t installed already then it will be
installed upon opening the `mortality-surveillance.Rproj` file.

To ensure versions used locally with this project are consistent, the
user must run the following line once in the console:

    renv::restore()

### Contributors

  - Sharmani Barnard, Senior Statistical Advisor
  - Sebastian Fox, Principal Data Scientist
  - Allan Baker, Deputy Head of Population Health Analysis
  - Paul Burton, Professor of Data Science for Health at Newcastle
    University and Honorary Consultant in Public Health (Epidemiology
    and Statistics) to PHE
  - Peter Goldblatt, Senior advisor University College London Institute
    of Health Equity and Statistical Advisor to PHE
  - Justine Fitzpatrick, Head of Population Health Analysis
