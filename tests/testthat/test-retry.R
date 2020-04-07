test_that("retry works", {
  expect_error(retry(10), "require")
  expect_equal(retry(10, when = "some error"), 10)
  expect_equal(retry(10, until = ~TRUE), 10)
  expect_silent(retry(message("hello"), when = "some error", silent = TRUE))
  expect_message(retry(message("hello"), when = "some error", silent = FALSE), "hello")
})

test_that("upon works", {
  s <- Sys.time()
  x <- 0
  expect_message(
    retry({
        message("hello")
        x <<- 1
      },
      upon = "message", until = ~ Sys.time() - s > 1),
    "hello")
  expect_equal(x, 0)
})

test_that("when works", {
  z <- 0
  f <- function() {
    if (z == 0) {
      z <<- 1
      stop("an error")
    }
    stop("another error")
  }
  expect_error(retry(f(), when = "an error"), "another error")
})

test_that("timeout works", {
  x <- 0
  later::later(function() x <<- 1, 0.5)
  expect_true(retry(TRUE, until = ~ x == 1, later_run_now = TRUE))
  expect_error(retry(stop("foo"), when = "foo", timeout = 0.1), "timeout")
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
