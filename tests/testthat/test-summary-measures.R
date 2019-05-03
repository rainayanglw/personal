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

  expect_equal(aux_skewness(10, 0.3), 0.2760262)
})

test_that("aux_kurtosis", {

  expect_equal(aux_kurtosis(10, 0.3), -0.1238095)
})


