library(testthat)
library(dplyr)
library(readr)
context("Checks that all values in period variable are valid.")

base_data <- read_csv("../data-raw/data.csv")

test_that(
  desc = "Period values are valid.",
  code = {
    all_period_values_valid <- all(base_data$period < 1000)
    expect_true(all_period_values_valid)
  }
)
