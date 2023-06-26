# Script that takes "cleaned" version of data ready for analysis in
# data/scbi.dendroAll_YEAR.csv and checks for errors that require field
# fixes listed in testthat/README.md.
#
# Developed by: Albert Y. Kim - albert.ys.kim@gmail.com
# R version 4.0.3 - First created in 2021
#
# 🔥HOT TIP🔥 Get a bird's eye view of what this code is doing by
# turning on "code folding" by going to RStudio menu -> Edit -> Folding
# -> Collapse all


# Set up ----
# clear environment
rm(list = ls())

# load libraries
library(here)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(ggplot2)
library(lubridate)
library(epitools)

## Load all master data files into a single data frame
<<<<<<< Updated upstream
master_data_filenames <- dir(path = here("data"), full.names = TRUE) #list.files(path="data")#dir(path = here("data"), pattern = "dendro*", full.names = TRUE)
=======
<<<<<<< HEAD
master_data_filenames <- dir(path = here("data"), full.names = TRUE)
=======
master_data_filenames <- dir(path = here("data"), full.names = TRUE) #list.files(path="data")#dir(path = here("data"), pattern = "dendro*", full.names = TRUE)
>>>>>>> 1ea81083597ca2423f65e5ac6601478407fb3cf9
>>>>>>> Stashed changes

dendroband_measurements_all_years <- NULL
for(i in 1:length(master_data_filenames)){
  dendroband_measurements_all_years <-
    bind_rows(
      dendroband_measurements_all_years,
      read_csv(master_data_filenames[i])
    )
}

## FOR TESTING
# dendroband_measurements_all_years <- read_csv("data/dendro_04_17_20.csv")

# Set years
current_year <- 2021 #Sys.Date() %>% year()
previous_year <- 2020#current_year - 1

# Get variable names (needed to write csv's consisting of only original variables)
orig_master_data_var_names <- names(dendroband_measurements_all_years)

# Run tests only on data from current year onwards
dendroband_measurements <- dendroband_measurements_all_years %>%
  filter(year == 2021)#current_year)

# Run all tests & checks ----
# prepare report files
require_field_fix_error_file <- NULL
will_auto_fix_error_file <- NULL
warning_file <- NULL


## Error: Status of stem is 1) not missing and 2) is "alive" or "dead"? ----
#alert_name <- "status_not_valid"

# Find stems with error
#stems_to_alert <- dendroband_measurements %>%
 # filter(!status %in% c("alive", "dead") | is.na(status))

# Append to report
#require_field_fix_error_file <- stems_to_alert %>%
#  mutate(alert_name = alert_name) %>%
#  select(alert_name, all_of(orig_master_data_var_names)) %>%
 # bind_rows(require_field_fix_error_file)



## Error: Is measure possible: between 0 & 150? ----
measure_limit <- 150
alert_name <- "measure_not_possible"

# Find stems with error
stems_to_alert <- dendroband_measurements %>%
  filter(!between(dend, 0, measure_limit))

# Append to report
require_field_fix_error_file <- stems_to_alert %>%
  mutate(alert_name = alert_name) %>%
  bind_rows(require_field_fix_error_file)


## Error: Is measure recorded: if measure is missing, then appropriate code must be entered ----
# Test that if measure is missing, then codes = RE is there
alert_name <- "measure_not_recorded"

# Find stems with error
stems_to_alert <- dendroband_measurements %>%
  mutate(missing_RE_code = !is.na(dend)) %>%
  #mutate(missing_RE_code = !is.na(dend) | str_detect(codes, regex("RE|DC|DS|DN|Q|B"))) %>%
  filter(!missing_RE_code)

# Append to report
require_field_fix_error_file <- stems_to_alert %>%
  mutate(alert_name = alert_name) %>%
  bind_rows(require_field_fix_error_file)



### Error: Anomaly detection for biannual: Is difference between new & previous measurement too big (unless new band is installed)? ----
#alert_name <- "new_measure_too_different_from_previous_biannual"
#
#if(!is.na(dendroband_measurements$dend)){
#  # Compute +/- 3SD of growth by species: used to detect anomalous growth below
#  previous_year_growth_by_sp <- dendroband_measurements_all_years %>%
#    # Only previous year spring and fall biannual values
#    filter(year == previous_year) %>%
#    filter(jday < 150 || jday > 260) %>%
#    # Compute growth
#    group_by(tag) %>%
#    mutate(growth = dend - lag(dend)) %>%
#    filter(!is.na(growth)) %>%
#    slice(n()) %>%
#    # 99.7% of values i.e. +/- 3 SD
#    group_by(sp) %>%
#    summarize(lower = quantile(growth, probs = 0.003/2), upper = quantile(growth, probs = 1-0.003/2), n = n()) %>%
#    arrange(desc(n))
#
#  # Get all measures that have been verified during fall survey
#  #verified_measures <-
#  #  fall_biannual_survey %>%
#  #  read_csv(show_col_types = FALSE) %>%
#  #  filter(measure_verified) %>%
#  #  select(tag, stemtag, sp, survey.ID, measure_verified)
#
#  # Get all stems to alert
#  stems_to_alert <- dendroband_measurements %>%
#    filter(survey.ID %in% c(spring_biannual_survey_ID, fall_biannual_survey_ID)) %>%
#    # Compute growth
#    group_by(tag, stemtag) %>%
#    mutate(growth = measure - lag(measure)) %>%
#    filter(!is.na(growth)) %>%
#    slice(n()) %>%
#    # See if growth is in 99.7% confidence interval
#    left_join(previous_year_growth_by_sp, by = "sp") %>%
#    mutate(measure_is_reasonable = between(growth, lower, upper)) %>%
#    filter(!measure_is_reasonable) %>%
#    # See if measure was verified, if so drop
#    left_join(verified_measures, by = c("tag", "stemtag", "sp", "survey.ID")) %>%
#    mutate(measure_verified = ifelse(is.na(measure_verified), FALSE, measure_verified)) %>%
#    filter(!measure_verified)
#
#  # Append to report
#  require_field_fix_error_file <- stems_to_alert %>%
#    mutate(alert_name = alert_name) %>%
#    select(alert_name, all_of(orig_master_data_var_names)) %>%
#    bind_rows(require_field_fix_error_file)
#}
#
#
# Display anomalies (if any) in README
#anomaly_plot_filename <- here("testthat/reports/measurement_anomalies.png")
#
#anamoly_dendroband_measurements <- dendroband_measurements %>%
#  filter(!is.na(measure) & tag %in% stems_to_alert$tag) %>%
#  mutate(stemtag = factor(stemtag)) %>%
#  mutate(tag_sp = str_c(tag, ": ", sp))
#
#if(nrow(anamoly_dendroband_measurements) > 0){
#  anomaly_plot <- anamoly_dendroband_measurements %>%
#    ggplot(aes(x = date, y = measure, col = stemtag)) +
#    geom_point() +
#    geom_line() +
#    geom_point(data = stems_to_alert, col = "black", size = 4, shape = 18) +
#    facet_wrap(~tag_sp, scales = "free_y") +
#    theme_bw() +
#    geom_vline(data = anamoly_dendroband_measurements %>% filter(new.band == 1), aes(xintercept = date)) +
#    labs(
#      x = "Biweekly survey date",
#      y = "Measure recorded",
#      title = "Stems with an anomalous measure: abs diff > 10mm, marked with diamond",
#      subtitle = "Dashed line = CI activation date, solid lines (if any) = new band install date"
#    )
#
#  ggsave(
#    filename = anomaly_plot_filename,
#    plot = anomaly_plot,
#    device = "png",
#    width = 16 / 2, height = (16/2)*(7/8),
#    units = "in", dpi = 300
#  )
#} else if (file.exists(anomaly_plot_filename)){
#  file.remove(anomaly_plot_filename)
#}



## Warning: Does dendroband needs fixing or replacing? ----
alert_name <- "dendroband_needs_fixing_or_replacing"

min_caliper_width <- 3
max_caliper_width <- 200

# Find stems with error
stems_to_alert <- dendroband_measurements %>%
  filter(!between(dend, min_caliper_width, max_caliper_width))

# Append to report
warning_file <- stems_to_alert %>%
  mutate(alert_name = alert_name) %>%
  bind_rows(warning_file)









# Clean and save files ----
## Field fix errors ----
report_filepath <- here("testthat/reports/requires_field_fix/require_field_fix_error_file.csv")
trace_of_reports_filepath <- here("testthat/reports/trace_of_reports/require_field_fix_error_file.csv")

if(nrow(require_field_fix_error_file) != 0){
  # If any field fix errors exist:

  # Clean & sort report
  require_field_fix_error_file <- require_field_fix_error_file %>%
    filter(!is.na(tag)) %>%
    arrange(plot, tag)

  # Write report
  require_field_fix_error_file %>%
    write_csv(file = report_filepath)

  # Append report to trace of reports to keep track of all the issues
  if(file.exists(trace_of_reports_filepath)){
    trace_of_reports <- read_csv(file = trace_of_reports_filepath, show_col_types = FALSE)
  } else {
    trace_of_reports <- NULL
  }

  trace_of_reports %>%
    bind_rows(require_field_fix_error_file) %>%
    distinct() %>%
    write_csv(file = trace_of_reports_filepath)

} else {
  # If no field fix errors exist, then delete previous report:
  if(file.exists(report_filepath)) {
    file.remove(report_filepath)
  }
}

## Warnings ----
report_filepath <- here("testthat/reports/warnings/warnings_file.csv")
trace_of_reports_filepath <- here("testthat/reports/trace_of_reports/warnings_file.csv")

if(nrow(warning_file) != 0){
  # If any warnings exist:

  # Clean & sort report
  warning_file <- warning_file %>%
    filter(!is.na(tag)) %>%
    arrange(alert_name, tag)

  # Write report
  warning_file %>%
    write_csv(file = report_filepath)

  # Append report to trace of reports to keep track of all the issues
  if(file.exists(trace_of_reports_filepath)){
    trace_of_reports <- read_csv(file = trace_of_reports_filepath, show_col_types = FALSE)
  } else {
    trace_of_reports <- NULL
  }

  trace_of_reports %>%
    bind_rows(warning_file) %>%
    distinct() %>%
    write_csv(file = trace_of_reports_filepath)

} else {
  # If no warnings exist, then delete previous report:
  if (file.exists(report_filepath)) {
    file.remove(report_filepath)
  }
}
