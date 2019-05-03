context("Summary measures")


test_that("aux_mean", {

  expect_equal(aux_mean(10, 0.3), 3)
})

test_that("aux_variance", {

  expect_equal(aux_variance(10, 0.3), 2.1)
})

test_that("aux_mode", {

  expect_equal(aux_mode(10, 0.3), 3)

})

test_that("aux_skewness", {

  expect_lt(aux_skewness(10, 0.3), 0.28)
  expect_gt(aux_skewness(10, 0.3), 0.27)
})

test_that("aux_kurtosis", {

  expect_lt(aux_kurtosis(10, 0.3), -0.122)
  expect_gt(aux_kurtosis(10, 0.3), -0.124)
})


