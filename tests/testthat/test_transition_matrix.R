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

# This is a somewhat pathological example since there are only zeros in the last row.
A.1 <- matrix(c(0, 1, 1, 1, 1,
                0, 0, 1, 1, 1,
                0, 0, 0, 1, 1,
                0, 0, 0, 0, 1,
                0, 0, 0, 0, 0), nrow = 5, ncol = 5, byrow = TRUE)
T.1 <- matrix(c(0, 1 / 4., 1 / 4., 1 / 4., 1 / 4.,
                0, 0, 1 / 3., 1 / 3., 1 / 3.,
                0, 0, 0, 1 / 2., 1 / 2.,
                0, 0, 0, 0, 1,
                1 / 4., 1 / 4., 1 / 4., 1 / 4., 0), nrow = 5, ncol = 5, byrow = TRUE)
A.2 <- matrix(c(0, 1, 2, 3,
                 1, 0, 1, 1,
                 10, 3, 0, 2,
                 5, 5, 0, 0), nrow = 4, ncol = 4, byrow = TRUE)
T.2 <- matrix(c(0, 1 / 6., 1 / 3., 1 / 2.,
                1 / 3., 0, 1 / 3., 1 / 3.,
                10 / 15., 3 / 15., 0, 2 / 15.,
                1 / 2., 1 / 2., 0, 0), nrow = 4, ncol = 4, byrow = TRUE)
test_that("output is correct", {
  expect_equal(ComputeTransitionMatrix(A.1), T.1, tolerance=1e-4)
  expect_equal(ComputeTransitionMatrix(A.2), T.2, tolerance=1e-4)
})

