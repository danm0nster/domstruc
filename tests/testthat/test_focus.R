library(testthat)
library(domstruc)

data("allee1954_3")

focus_compare <- 0.7268178975399129

test_that("Focus reproduces correct result", {
  expect_equal(dom_focus(allee1954_3),
               focus_compare,
               tolerance = 1e-6)
})
