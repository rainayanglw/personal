context("Binomial")


test_that("bin_choose", {

  expect_equal(bin_choose( 5, 2), 10)
  expect_equal(bin_choose( 5, 0), 1)
})

test_that("bin_probability", {

  expect_equal(bin_probability( 2, 5, 0.5), 0.3125)
  expect_equal(bin_probability( 0:2, 5, 0.5), c(0.03125, 0.15625, 0.31250))
})

test_that("bin_distribution", {

  expect_equal(bin_distribution( 5, 0.5), data.frame("success" = c(0:5), "probability" = c(0.03125,
                                                                                        0.15625,
                                                                                        0.31250,
                                                                                        0.31250,
                                                                                        0.15625,
                                                                                        0.03125)))
})

test_that("bin_cumulative", {
  expect_equal(bin_cumulative( 5, 0.5), data.frame(success = c(0:5), probability = c(0.03125,
                                                                                      0.15625,
                                                                                      0.31250,
                                                                                      0.31250,
                                                                                      0.15625,
                                                                                      0.03125), cumulative = c(0.03125, 0.18750,0.50000,0.81250,0.96875,1.00000)))
})




