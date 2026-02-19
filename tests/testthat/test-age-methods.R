library(testthat)

source('../../who-validation.R')

test_that("head circumference only computed for <61 months", {
  ages_months <- c(50, 61, 80)
  df <- data.frame(start_date = NA_character_, end_date = NA_character_, age_days = as.integer(round(ages_months * 30.4375)), age_months = ages_months)

  res <- compute_measurements(df, measurement_method = "headc", requested_z = 0)

  # first row should have a value, others should be NA
  expect_true(!is.na(res$headc[1]))
  expect_true(is.na(res$headc[2]))
  expect_true(is.na(res$headc[3]))
})

test_that("weight only computed for ages <= 120 months", {
  ages_months <- c(120, 121)
  df <- data.frame(start_date = NA_character_, end_date = NA_character_, age_days = as.integer(round(ages_months * 30.4375)), age_months = ages_months)

  res <- compute_measurements(df, measurement_method = "weight", requested_z = 0)

  expect_true(!is.na(res$weight_kg[1]))
  expect_true(is.na(res$weight_kg[2]))
})

test_that("bmi computed for all ages", {
  ages_months <- c(50, 100, 200)
  df <- data.frame(start_date = NA_character_, end_date = NA_character_, age_days = as.integer(round(ages_months * 30.4375)), age_months = ages_months)

  res <- compute_measurements(df, measurement_method = "bmi", requested_z = 0)

  expect_true(!any(is.na(res$bmi)))
})
