test_that("retry works", {
  expect_equal(retry(10), 10)
  expect_silent(retry(message("hello")))
  expect_message(retry(message("hello"), silent = FALSE), "hello")
  expect_equal(retry(10, until = ~TRUE), 10)
})

test_that("timeout works", {
  x <- 0
  later::later(function() x <<- 1, 0.5)
  expect_true(retry(later::run_now() || TRUE, until = ~ x == 1, timeout = 5))
  expect_error(retry(stop(), timeout = 0.1), "timeout")
})

test_that("max_tries works", {
  x <- 0
  counter <- function() {
    x <<- x + 1
    x
  }
  expect_error(
    retry(counter(), until = ~FALSE, max_tries = 10),
    "maximum number of tries exceeded")
  expect_equal(x, 10)
})

test_that("wait_until works", {
    x <- 0
    later::later(function() x <<- 1, 0.5)
    wait_until(x == 1, timeout = 5)
    expect_equal(x, 1)
})
