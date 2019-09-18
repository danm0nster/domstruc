library(testthat)
library(domstruc)

data("allee1954_3")

position_compare <- 0.4235290014009454

test_that("Position reproduces correct result", {
  expect_equal(dom_position(allee1954_3),
               position_compare,
               tolerance = 1e-6)
})
