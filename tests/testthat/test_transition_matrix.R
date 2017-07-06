library(testthat)
library(domstruc)

context("Transition matrix")

test_that("missing argument gives error", {
  expect_error(ComputeTransitionMatrix(), "Please provide an aggression matrix as input.")
})

A.matrix <- matrix(data = c(2,1,0,4,5,1,3,4,1), nrow = 3, ncol = 3)

test_that("invalid values of eps gives error", {
  expect_error(ComputeTransitionMatrix(A.matrix, eps = 2), "eps must be less than or equal to 1.")
  expect_error(ComputeTransitionMatrix(A.matrix, eps = -1), "eps must be positive.")
  expect_error(ComputeTransitionMatrix(A.matrix, eps = 1e-100), "You specified eps less that machine precision. Please increase value.")
})

non.square.A <- matrix(data = c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
test_that("matrix dimension is checked", {
  expect_error(ComputeTransitionMatrix(non.square.A), "Aggression matrix must be square.")
  expect_error(ComputeTransitionMatrix(as.matrix(0)), "Aggression matrix dimension must be greater than 1.")
})
