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

master_data_filenames <- dir(path = "data", full.names = TRUE)
dendroband_measurements_all_years <- NULL
for(i in 1:length(master_data_filenames)){
  dendroband_measurements_all_years <-
    bind_rows(
      dendroband_measurements_all_years,
      read_csv(master_data_filenames[i])
    )
}

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
alert_name <- "new_measure_too_different_from_sp_growth_rate"

# calculate growth rates for previous year by species
# create a fall and spring survey dataset
spring <- filter(dendroband_measurements_all_years, jday < 150 & year == previous_year)
fall <- filter(dendroband_measurements_all_years, jday > 260 & year == previous_year)

# merge spring and fall datasets based on ID number to isolate only trees with values in both censuses
growth_rates <- inner_join(spring, fall, by = c("tag", "plot", "dend.num"))

# calculate time difference and convert time from days to years
time <- (growth_rates$jday.y-growth_rates$jday.x)/365

# assign dend at time 1 (size1) and time 2 (size2)
size2 <- growth_rates$dend.y
size1 <- growth_rates$dend.x

# calculate growth rates:
growth_rates$annual_increment <- (size2 - size1)

# group by species
growth_sp <- growth_rates %>%
  group_by(spp.x) %>%
  summarize(growth_rate = mean(annual_increment, na.rm =TRUE),
            upper = growth_rate + 3*sd(annual_increment, na.rm = TRUE),
            lower = growth_rate - 3*sd(annual_increment, na.rm = TRUE),
            n = n())

spring <- filter(dendroband_measurements, jday < 150)
other <- filter(dendroband_measurements, jday >= 150)
# Get all stems to alert
stems_to_alert <- other %>%
  # Compute growth
  left_join(spring, by = c("plot","tag", "dend.num"), relationship = "many-to-many")%>%
  group_by(tag, plot) %>%
  mutate(growth = dend.x - dend.y) %>%
  filter(!is.na(growth)) %>%
  slice(n()) %>%
  # See if growth is in 99.7% confidence interval
  left_join(growth_sp, by = "spp.x") %>%
  mutate(measure_is_reasonable = between(growth, lower, upper)) %>%
  filter(!measure_is_reasonable)

# Append to report
require_field_fix_error_file <- stems_to_alert %>%
  mutate(alert_name = alert_name) %>%
  select(alert_name, all_of(orig_master_data_var_names)) %>%
  bind_rows(require_field_fix_error_file)


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
