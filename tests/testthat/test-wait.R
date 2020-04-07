test_that("wait_until works", {
    x <- 0
    later::later(function() x <<- 1, 0.5)
    wait_until(x == 1, timeout = 5)
    expect_equal(x, 1)
})
