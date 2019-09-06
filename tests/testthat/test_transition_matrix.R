library(testthat)
library(domstruc)

context("Transition matrix")

test_that("missing argument gives error", {
  expect_error(
    transition_matrix(),
    "Please provide an aggression matrix as input."
  )
})

a_matrix <- matrix(
  data = c(0, 1, 2, 4, 0, 1, 3, 4, 0),
  nrow = 3, ncol = 3, byrow = TRUE
)

test_that("wrong argument type raises an error", {
  expect_error(
    transition_matrix(1),
    "Argument must be a matrix."
  )
  expect_error(
    transition_matrix(a_matrix, epsilon = c(0.1, 0.2)),
    "eps must be a scalar."
  )
})

test_that("invalid values of eps gives error", {
  expect_error(
    transition_matrix(a_matrix, epsilon = 2),
    "eps must be less than or equal to 1."
  )
  expect_error(
    transition_matrix(a_matrix, epsilon = -1),
    "eps must be positive."
  )
  expect_error(
    transition_matrix(a_matrix, epsilon = 1e-100),
    paste0(
      "You specified eps less that machine precision.\n",
      "Please increase value."
    )
  )
})


non_square <- matrix(data = c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)

test_that("matrix dimension is checked", {
  expect_error(
    transition_matrix(non_square),
    "Aggression matrix must be square."
  )
  expect_error(
    transition_matrix(as.matrix(0)),
    "Aggression matrix dimension must be greater than 1."
  )
})

non_zero_trace <- matrix(c(1, 1, 2, 0), nrow = 2, ncol = 2, byrow = TRUE)

test_that("non-zero trace is checked", {
  expect_error(
    transition_matrix(non_zero_trace),
    "Aggression matrix has non-zero trace. Self-aggression?"
  )
})

# This is a somewhat pathological example,
# since there are only zeros in the last row.
a_1 <- matrix(c(
  0, 1, 1, 1, 1,
  0, 0, 1, 1, 1,
  0, 0, 0, 1, 1,
  0, 0, 0, 0, 1,
  0, 0, 0, 0, 0
), nrow = 5, ncol = 5, byrow = TRUE)
t_1 <- matrix(c(
  0, 1 / 4., 1 / 4., 1 / 4., 1 / 4.,
  0, 0, 1 / 3., 1 / 3., 1 / 3.,
  0, 0, 0, 1 / 2., 1 / 2.,
  0, 0, 0, 0, 1,
  1 / 4., 1 / 4., 1 / 4., 1 / 4., 0
), nrow = 5, ncol = 5, byrow = TRUE)
a_2 <- matrix(c(
  0, 1, 2, 3,
  1, 0, 1, 1,
  10, 3, 0, 2,
  5, 5, 0, 0
), nrow = 4, ncol = 4, byrow = TRUE)
t_2 <- matrix(c(
  0, 1 / 6., 1 / 3., 1 / 2.,
  1 / 3., 0, 1 / 3., 1 / 3.,
  10 / 15., 3 / 15., 0, 2 / 15.,
  1 / 2., 1 / 2., 0, 0
), nrow = 4, ncol = 4, byrow = TRUE)

# Bromley1991-1.csv, from Dryad data set
# collected by Shikuza & McDonald
a_4 <- matrix(c(
  0, 7, 2, 13, 10, 2, 4, 0,
  1, 0, 10, 0, 2, 3, 6, 1,
  1, 3, 0, 2, 3, 0, 5, 0,
  0, 0, 1, 0, 8, 1, 2, 1,
  3, 0, 0, 1, 0, 6, 13, 5,
  0, 0, 0, 0, 0, 0, 3, 3,
  0, 0, 0, 0, 0, 2, 0, 4,
  0, 0, 0, 0, 0, 1, 1, 0
), nrow = 8, ncol = 8, byrow = TRUE)
t_4 <- matrix(c(
  0, 0.1842, 0.0526, 0.3421, 0.2632, 0.0526, 0.1053, 0.0000,
  0.0435, 0, 0.4348, 0, 0.0870, 0.1304, 0.2609, 0.0435,
  0.0714, 0.2143, 0, 0.1429, 0.2143, 0, 0.3571, 0.0000,
  0, 0, 0.0769, 0, 0.6154, 0.0769, 0.1538, 0.0769,
  0.1071, 0, 0, 0.0357, 0, 0.2143, 0.4643, 0.1786,
  0, 0, 0, 0, 0, 0, 0.5000, 0.5000,
  0, 0, 0, 0, 0, 0.3333, 0, 0.6667,
  0, 0, 0, 0, 0, 0.5000, 0.5000, 0.0000
), nrow = 8, ncol = 8, byrow = TRUE)
test_that("output is correct", {
  expect_equal(transition_matrix(a_1, epsilon = 1e-12), t_1, tolerance = 1e-4)
  expect_equal(transition_matrix(a_2, epsilon = 1e-12), t_2, tolerance = 1e-4)
  expect_equal(transition_matrix(a_4, epsilon = 1e-12), t_4, tolerance = 1e-4)
})

test_that("two-dimensional array (matrix) works", {
  expect_equal(
    transition_matrix(array(c(0, 1, 2, 0), dim = c(2, 2)), epsilon = 1e-12),
    array(c(0, 1, 1, 0), dim = c(2, 2))
  )
})

a_neg <- matrix(1, nrow = 3, ncol = 3)
diag(a_neg) <- c(0, 0, 0)
a_neg[1, 2] <- -1
test_that("negative elements raise an error", {
  expect_error(
    transition_matrix(a_neg),
    "Negative element detected in aggression matrix."
  )
})
