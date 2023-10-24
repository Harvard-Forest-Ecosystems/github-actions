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

master_data_filenames <- list.files(path = "new_data",  full.names = TRUE)
dendroband_measurements_all_years <- NULL
for(i in 1:length(master_data_filenames)){
  file <- read_csv(master_data_filenames[i])
  file$dend <- as.numeric(file$dend)
  file$prev.dend <- as.numeric(file$prev.dend)
  dendroband_measurements_all_years <-
    bind_rows(
      dendroband_measurements_all_years,
      file
    )
}

files_2022 <- list.files(path = 'data', pattern = '*2022', full.names = TRUE)
data_2022 <- NULL
for(i in 1:length(files_2022)){
  file <- read_csv(files_2022[i])
  file$dend <- as.numeric(file$dend)
  data_2022 <-
    bind_rows(
      data_2022,
      file
    )
}

# Set years
current_year <- Sys.Date() %>% year()
previous_year <- current_year - 1

# Get variable names (needed to write csv's consisting of only original variables)
orig_master_data_var_names <- names(dendroband_measurements_all_years)

# Run tests only on data from current year onwards
dendroband_measurements <- dendroband_measurements_all_years %>%
  filter(year == current_year)

# Run all tests & checks ----
# prepare report files
require_field_fix_error_file <- NULL
will_auto_fix_error_file <- NULL
warning_file <- NULL

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
  mutate(missing_RE_code = !is.na(dend) | str_detect(hole, regex('No Measurement')) | str_detect(status, regex('Dead'))) %>%
  filter(!missing_RE_code)

# Append to report
require_field_fix_error_file <- stems_to_alert %>%
  mutate(alert_name = alert_name) %>%
  bind_rows(require_field_fix_error_file)


### Error: Anomaly detection for biannual: Is difference between new & previous measurement too big----
alert_name <- "new_measure_too_different_from_sp_growth_rate"

# calculate growth rates for previous year by species
# create a fall and spring survey dataset
spring <- filter(data_2022, jday < 150 & year == previous_year)
fall <- filter(data_2022, jday > 260 & year == previous_year)

# merge spring and fall datasets based on ID number to isolate only trees with values in both censuses
growth_rates <- inner_join(spring, fall, by = c("tag", "plot", "dend.num"))

# calculate time difference and convert time from days to years
growth_rates <- growth_rates %>%
  mutate(annual_increment = (dend.y-prev.dend.y))

# group by species
growth_sp <- growth_rates %>%
  group_by(spp.x) %>%
  summarize(growth_rate = mean(annual_increment, na.rm =TRUE),
            upper = growth_rate + 3*sd(annual_increment, na.rm = TRUE),
            lower = growth_rate - 3*sd(annual_increment, na.rm = TRUE),
            n = n())

growth_sp <- rename(growth_sp,
                    'spp' = 'spp.x')

# Get all stems to alert
stems_to_alert <- dendroband_measurements %>%
  # Compute growth
  mutate(growth = (dend - prev.dend)) %>%
  filter(!is.na(growth)) %>%
  # See if growth is in 99.7% confidence interval
  left_join(growth_sp, by = "spp") %>%
  mutate(measure_is_reasonable = between(growth, lower, upper)) %>%
  filter(!measure_is_reasonable)

# Append to report
require_field_fix_error_file <- stems_to_alert %>%
  mutate(alert_name = alert_name) %>%
  dplyr::select(alert_name, all_of(orig_master_data_var_names)) %>%
  bind_rows(require_field_fix_error_file)


## Warning: Does dendroband needs fixing or replacing? ----
alert_name <- "dendroband_needs_fixing_or_replacing"

min_caliper_width <- 3
max_caliper_width <- 150

# Find stems with error
stems_to_alert <- dendroband_measurements %>%
  filter(!between(dend, min_caliper_width, max_caliper_width))

# Append to report
warning_file <- stems_to_alert %>%
  mutate(alert_name = alert_name) %>%
  bind_rows(warning_file)




# save report file
write_csv(warning_file, "testthat/reports/trace_of_reports/require_field_fix_error_file.csv")



# Clean and save files ----
## Field fix errors ----
#report_filepath <- "/Users/runner/work/github-actions/github-actions/testthat/reports/requires_field_fix/require_field_fix_error_file.csv"
#trace_of_reports_filepath <- "/Users/runner/work/github-actions/github-actions/testthat/reports/trace_of_reports/require_field_fix_error_file.csv"

#if(nrow(require_field_fix_error_file) != 0){
  # If any field fix errors exist:

  # Clean & sort report
#  require_field_fix_error_file <- require_field_fix_error_file %>%
#    filter(!is.na(tag)) %>%
#    arrange(plot, tag)

  # Write report
#  require_field_fix_error_file %>%
 #   write_csv(file = report_filepath)

  # Append report to trace of reports to keep track of all the issues
#  if(file.exists(trace_of_reports_filepath)){
#    trace_of_reports <- read_csv(file = trace_of_reports_filepath, show_col_types = FALSE)
#  } else {
#    trace_of_reports <- NULL
#  }

#  trace_of_reports %>%
#    bind_rows(require_field_fix_error_file) %>%
#    distinct() %>%
#    write_csv(file = trace_of_reports_filepath)

#} else {
  # If no field fix errors exist, then delete previous report:
#  if(file.exists(report_filepath)) {
#    file.remove(report_filepath)
#  }
#}

## Warnings ---
#report_filepath <- "/Users/runner/work/github-actions/github-actions/testthat/reports/warnings/warnings_file.csv"
#trace_of_reports_filepath <- "/Users/runner/work/github-actions/github-actions/testthat/reports/trace_of_reports/warnings_file.csv"

#if(nrow(warning_file) != 0){
  # If any warnings exist:

  # Clean & sort report
#  warning_file <- warning_file %>%
#    filter(!is.na(tag)) %>%
#    arrange(alert_name, tag)

  # Write report
#  warning_file %>%
#    write_csv(file = report_filepath)

  # Append report to trace of reports to keep track of all the issues
#  if(file.exists(trace_of_reports_filepath)){
#    trace_of_reports <- read_csv(file = trace_of_reports_filepath, show_col_types = FALSE)
#  } else {
#    trace_of_reports <- NULL
#  }

#  trace_of_reports %>%
#    bind_rows(warning_file) %>%
#    distinct() %>%
#    write_csv(file = trace_of_reports_filepath)

#} else {
#  # If no warnings exist, then delete previous report:
#  if (file.exists(report_filepath)) {
#    file.remove(report_filepath)
#  }
#}
