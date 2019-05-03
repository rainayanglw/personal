context("checkers")


test_that("check_prob works with ok vectors", {

  expect_true(check_prob(c(0.5, 0.5)))
  expect_true(check_prob(c(0, 1)))
  expect_true(check_prob(c(1, 0)))
  expect_true(check_prob(c(0.1, 0.9)))
  expect_true(check_prob(c(1/3, 2/3)))
  expect_true(check_prob(c(1/6, 5/6)))
})


test_that("check_prob fails with invalid numbers", {

  expect_error(check_prob(0.333, 0.666))
  expect_error(check_prob(-0.5, 0.5))
  expect_error(check_prob(0.5, -0.5))
  expect_error(check_prob(0.5, NA))
})

test_that("check_trials", {

  expect_true(check_trials(c(1:3)))
  expect_true(check_trials(18))
  expect_error(check_trials(0.8))
})

test_that("check_success", {

  expect_true(check_success(c(2,3), 5))
  expect_true(check_success(18, 30))
  expect_error(check_success(0.8, 0.1))
  expect_error(check_success(10, 5))

})
