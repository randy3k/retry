test_that("retry", {
  expect_equal(retry(10), 10)
  expect_equal(retry(10, until = ~TRUE), 10)
  x <- 0
  later::later(function() x <<- 1, 0.5)
  expect_true(retry(later::run_now() || TRUE, until = ~ x == 1))
  expect_error(retry(stop(), timeout = 0.1), "timeout")
})
