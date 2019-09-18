library(testthat)
library(domstruc)

data("allee1954_3")


pure_down_null_delta_ranks <-
  c(
    -1,
    -2,
    1,
    -4,
    -3,
    3,
    4,
    2,
    5,
    6,
    7,
    -5,
    8,
    9,
    -7,
    -6,
    -8,
    -9
  )

pure_down_null_diff <-
  c(
    0.0,
    0.0,
    71.67738095238096,
    0.0,
    0.0,
    46.17738095238095,
    44.17738095238095,
    58.67738095238095,
    38.17738095238095,
    3.577380952380952,
    1.4107142857142856,
    0.0,
    1.125,
    0.0,
    0.0,
    0.0,
    0.0,
    0.0
  )

pure_down_null_diff_count <-
  c(
    265.0,
    256.0,
    265.0,
    241.0,
    254.0,
    227.0,
    221.0,
    252.0,
    197.0,
    24.0,
    11.0,
    68.0,
    9.0,
    0.0,
    38.0,
    44.0,
    13.0,
    0.0
  )

pure_down_null_test <-
  data.frame(delta = pure_down_null_delta_ranks,
             agg = pure_down_null_diff,
             agg_norm = pure_down_null_diff_count)

pure_down_null_result <- dom_rank_focused_aggression(
  dom_make_downward_null(allee1954_3, blur = 0))

# Sort the data.frames by delta rank before comparing to ensure they are in the
# same order.
pure_down_null_result <- pure_down_null_result[order(pure_down_null_result$delta), ]
pure_down_null_test <- pure_down_null_test[order(pure_down_null_test$delta), ]

# Reset the row names so the test does not fail because they differ
rownames(pure_down_null_result) <- NULL
rownames(pure_down_null_test) <- NULL

test_that("Rank focused aggression for pure downward null is correct", {
  expect_equal(pure_down_null_result,
               pure_down_null_test)
})
