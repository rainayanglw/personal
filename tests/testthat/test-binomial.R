context("Binomial")


test_that("bin_choose works", {

  expect_equal(bin_choose( 5, 2), 10)
  expect_equal(bin_choose( 5, 0), 1)
})

test_that("bin_probability works", {

  expect_equal(bin_probability( 2, 5, 0.5), 0.3125)
  expect_equal(bin_probability( 0:2, 5, 0.5), c(0.03125, 0.15625, 0.31250))
})

test_that("bin_distribution works", {
  expect_is(bin_distribution(5, 0.5),c("bindis","data.frame"))

  expect_length(bin_distribution( 5, 0.5), 2)
})

test_that("Bin cumulative works",{
  expect_is(bin_cumulative(5, 0.5),c("bincum","data.frame"))
  expect_length(bin_cumulative(5, 0.5),3)
  expect_error(bin_cumulative(-5, 0.5))
})



