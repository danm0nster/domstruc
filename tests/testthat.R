library(testthat)
library(domstruc)
library(lintr)

test_check("domstruc")
path <- getwd()

if (requireNamespace("lintr", quietly = TRUE)) {
  test_that("Package Style", {
    lintr::expect_lint_free(path = path, relative_path = FALSE)
  })
}
