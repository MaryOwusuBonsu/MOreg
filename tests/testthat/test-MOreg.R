library(testthat)
library(MOreg)

test_that("MOreg produces correct coefficients", {
  Y <- c(2.5, 6.8, 8.7, 9)
  X <- c(15, 25, 30, 29)

  # Run MOreg
  model_mo <- MOreg(Y, X)

  # Run lm
  data <- data.frame(Y = Y, X = X)
  model_lm <- lm(Y ~ X, data = data)

  # Compare coefficients
  coef_mo <- as.vector(model_mo$beta)
  coef_lm <- unname(coef(model_lm))

  expect_equal(coef_mo, coef_lm, tolerance = 1e-6)
})

test_that("MOreg produces correct residuals", {
  Y <- c(2.5, 6.8, 8.7, 9)
  X <- c(15, 25, 30, 29)

  # Run MOreg
  model_mo <- MOreg(Y, X)

  # Run lm
  data <- data.frame(Y = Y, X = X)
  model_lm <- lm(Y ~ X, data = data)

  # Compare residuals
  resid_mo <- unname(as.vector(model_mo$residuals))
  resid_lm <- unname(residuals(model_lm))

  expect_equal(resid_mo, resid_lm, tolerance = 1e-6)
})

test_that("MOreg produces correct sigma_sq_hat", {
  Y <- c(2.5, 6.8, 8.7, 9)
  X <- c(15, 25, 30, 29)

  # Run MOreg
  model_mo <- MOreg(Y, X)

  # Run lm
  data <- data.frame(Y = Y, X = X)
  model_lm <- lm(Y ~ X, data = data)

  # Compare sigma_sq_hat
  sigma_sq_mo <- model_mo$sigma_sq_hat
  sigma_sq_lm <- summary(model_lm)$sigma^2

  expect_equal(sigma_sq_mo, sigma_sq_lm, tolerance = 1e-6)
})

test_that("MOreg produces correct standard errors", {
  Y <- c(2.5, 6.8, 8.7, 9)
  X <- c(15, 25, 30, 29)

  # Run MOreg
  model_mo <- MOreg(Y, X)

  # Run lm
  data <- data.frame(Y = Y, X = X)
  model_lm <- lm(Y ~ X, data = data)

  # Compare standard errors
  std_err_mo <- as.vector(model_mo$std_err)
  std_err_lm <- unname(summary(model_lm)$coefficients[, "Std. Error"])

  expect_equal(std_err_mo, std_err_lm, tolerance = 1e-6)
})

test_that("MOreg produces correct t-values", {
  Y <- c(2.5, 6.8, 8.7, 9)
  X <- c(15, 25, 30, 29)

  # Run MOreg
  model_mo <- MOreg(Y, X)

  # Run lm
  data <- data.frame(Y = Y, X = X)
  model_lm <- lm(Y ~ X, data = data)

  # Compare t-values
  tval_mo <- as.vector(model_mo$tval)
  tval_lm <- unname(summary(model_lm)$coefficients[, "t value"])

  expect_equal(tval_mo, tval_lm, tolerance = 1e-6)
})

test_that("MOreg produces correct p-values", {
  Y <- c(2.5, 6.8, 8.7, 9)
  X <- c(15, 25, 30, 29)

  # Run MOreg
  model_mo <- MOreg(Y, X)

  # Run lm
  data <- data.frame(Y = Y, X = X)
  model_lm <- lm(Y ~ X, data = data)

  # Compare p-values
  pval_mo <- as.vector(model_mo$pval)
  pval_lm <- unname(summary(model_lm)$coefficients[, "Pr(>|t|)"])

  expect_equal(pval_mo, pval_lm, tolerance = 1e-6)
})
