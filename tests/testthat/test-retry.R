test_that("retry works", {
  expect_equal(retry(10), 10)
  expect_equal(retry(10, until = ~TRUE), 10)
  x <- 0
  later::later(function() x <<- 1, 0.5)
  expect_true(retry(later::run_now() || TRUE, until = ~ x == 1, timeout = 5))
  expect_error(retry(stop(), timeout = 0.1), "timeout")
})

test_that("wait_until works", {
    x <- 0
    later::later(function() x <<- 1, 0.5)
    wait_until(x == 1, timeout = 5)
    expect_equal(x, 1)
})
