context("Test rlassoIVselectZ")
library(hdm)
library(testthat)

# IV model without controls; y and d have nonzero means, true effect is -2
set.seed(1)
n <- 2000
z <- cbind(z1 = rnorm(n), z2 = rnorm(n))
u <- rnorm(n)
d <- 5 + 0.8*z[, 1] + 0.5*z[, 2] + 0.5*u + rnorm(n)
y <- 10 - 2*d + u


test_that("rlassoIVselectZ - second stage includes an intercept", {
  fit <- rlassoIV(x = NULL, d = d, y = y, z = z, select.X = FALSE, select.Z = TRUE)
  # both instruments are selected, so the estimator is 2SLS with an intercept
  expect_true(all(fit$selection.matrix))
  ref <- tsls(x = NULL, d = d, y = y, z = z, intercept = TRUE, homoscedastic = FALSE)
  expect_equal(unname(fit$coefficients), unname(ref$coefficients[1, 1]), tolerance = 1e-8)
  expect_equal(unname(fit$se), unname(ref$se[1]), tolerance = 1e-8)
  expect_equal(unname(fit$coefficients), -2, tolerance = 0.1)
})

test_that("rlassoIVselectZ - estimate does not depend on the location of y", {
  fit <- rlassoIVselectZ(x = NULL, d = d, y = y, z = z)
  fit.shift <- rlassoIVselectZ(x = NULL, d = d, y = y + 100, z = z)
  expect_equal(fit.shift$coefficients, fit$coefficients, tolerance = 1e-8)
  expect_equal(fit.shift$se, fit$se, tolerance = 1e-8)
})
