suppressMessages(source("R/libraries.R"))
source("R/function_predictions.R")
source("R/function_visualisations.R")
source("R/utils.R")
readRenviron(".Renviron")
source("R/function_monthly_populations.R")
source("tests/test-assertr.R")
source("R/function_deaths_data.R")
source("R/collate_nat_exmort.R")
source("R/create_report_data.R")
source("R/email.R")


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


if (Sys.getenv("USERNAME") %in% c("sebastian.fox", "sam.dunn"))
  send_email(subject = "AUTO-EMAIL: National report is beginning", 
             Sys.getenv("USERNAME"),
             report_type = report_type_setting)

complete_status <- generate_report_data(final_date = final_date,
                                        eth_dep_setting = eth_dep_setting,
                                        age_group_type_setting = age_group_type_setting,
                                        pop_type_setting = pop_type_setting,
                                        from_date = from_date,
                                        report_type = report_type_setting)

objects_that_are_here <- complete_status$objects_that_are_here
is_everything_here <- complete_status$is_everything_here

# If everything exists, copy the data files into the file format used by Power BI
# Then the total numbers in each subgroup are compared
# Then the data file is produced

# Otherwise, print out which objects are missing

if (is_everything_here) {
  if (Sys.getenv("USERNAME") %in% c("sebastian.fox", "sam.dunn")) 
    send_email(subject = "AUTO-EMAIL: National report ran successfully", 
               Sys.getenv("USERNAME"),
               report_type = report_type_setting)
  
  path <- collate_powerbi_files_for_powerbi(geography = "england", 
                                            final_date = final_date,
                                            report_type = report_type_setting)  
}
  
  # QA the numbers ----------------------------------------------------------
  # bear in mind, all persons, age group, region, ethnicity-sex, deprivation are now all the same model
  # this table shows the difference between the cumulative all persons deaths (registered, expected and covid) compared to the totals within each subgroup
  qa <- qa_power_bi_file(path)
  
  if (!Sys.getenv("USERNAME") %in% c("sebastian.fox", "sam.dunn")) View(qa) # Add in not in
  
  write.csv(qa,
            paste0(Sys.getenv("POWERBI_FILESHARE"),
                   "/qa/compared_to_all_persons_",
                   gsub("-", "", as.character(final_date)),
                   live_or_test_suffix,
                   ".csv"),
            row.names = FALSE)
  
  # This check removed the counts for the final week and compares the totals with the previous week csv file
  # Bear in mind:
  # - cause of deaths gets updated on the death certificate over time, so most of the differences are related to 
  #    covid deaths and other causes of deaths being assigned
  compared_to_last_week <- compare_this_and_last_weeks_file(path)
  
  if (!Sys.getenv("USERNAME") %in% c("sebastian.fox", "sam.dunn"))  View(compared_to_last_week) # add in not in
  
  write.csv(compared_to_last_week,
            paste0(Sys.getenv("POWERBI_FILESHARE"),
                   "/qa/compared_to_last_week_",
                   gsub("-", "", as.character(final_date)),
                   live_or_test_suffix,
                   ".csv"),
            row.names = FALSE)
  
  
  # Create an excel file containing all of the data for public use --------
  
  xlsx_file <- create_excel_file(input_filepath = path,
                                 output_filepath = paste0(Sys.getenv("POWERBI_FILESHARE"),
                                                          "/EMData", 
                                                          live_or_test_suffix,
                                                          ".xlsx"))
  
  
  # Install RDCOMClient from binaries ---------------------------------------
  
  if (!require(RDCOMClient)) {
    #url <- "http://www.omegahat.net/R/bin/windows/contrib/4.0.0/RDCOMClient_0.94-0.zip"
    #install.packages(url, repos = NULL, type = "binary")
    devtools::install_github("omegahat/RDCOMClient", 
                             ref = "cf00f61") # could create snap shot with this. - library command line 98
  }
  detach("package:RDCOMClient", unload = TRUE)
  
  if (Sys.getenv("USERNAME") == "sam.dunn") library(RDCOMClient, lib.loc = paste0("C:/Users/",Sys.getenv("USERNAME"),"/Documents/R/win-library/4.0")) # wouldn't need if added to renv.
  
  library(RDCOMClient)
  
  # Convert the Excel file to ods -------------------------------------------
  convert_to_ods(xlsx_file)
  
  # move ods file to E&S fileshare
  # archive existing file
  
  old_filename <- paste0(Sys.getenv("E_AND_S_FILESHARE"),
                         "EMData",
                         date_as_string(path, 
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
                          "/EMData", 
                          live_or_test_suffix,
                          ".ods"),
            to = current_filename,
            overwrite = TRUE)
  
  if (Sys.getenv("USERNAME") %in% c("sebastian.fox", "sam.dunn")) {
    send_email(subject = "AUTO-EMAIL: National report all files generated successfully",
             include_success_attachments = TRUE, 
             Sys.getenv("USERNAME"),
             report_type = report_type_setting)
  
  
} else {
  if (Sys.getenv("USERNAME") %in%  c("sebastian.fox", "sam.dunn")) 
    send_email(subject = "AUTO-EMAIL: National report failed",
               Sys.getenv("USERNAME"),
               report_type = report_type_setting)
  
  missing_objects <- names(objects_that_are_here)[objects_that_are_here == FALSE]
  if (length(missing_objects) == 1) {
    word <- "is"
  } else {
    word <- "are"
  }
  print(paste(paste(missing_objects, collapse = ", "),
              word,
              "missing"))
}



