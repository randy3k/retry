test_that("retry", {
  expect_equal(retry(10), 10)
  expect_error(retry(stop(), timeout = 0.1), "timeout")
})
