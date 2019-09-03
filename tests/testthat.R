library(testthat)
library(domstruc)
library(lintr)

test_check("domstruc")

if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    lintr::expect_lint_free()
  })
}
