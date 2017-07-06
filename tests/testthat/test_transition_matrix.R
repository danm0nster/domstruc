library(testthat)
library(domstruc)

context("Transition matrix")

test_that("Error on missing argument", {
  expect_error(ComputeTransitionMatrix(), "Please provide an aggression matrix as input.")
})
