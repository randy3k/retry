
# retry

[![Github
Action](https://github.com/randy3k/retry/workflows/build/badge.svg?branch=master)](https://github.com/randy3k/retry)
[![codecov](https://codecov.io/gh/randy3k/retry/branch/master/graph/badge.svg)](https://codecov.io/gh/randy3k/retry)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/retry)](https://cran.r-project.org/package=retry)
[![](http://cranlogs.r-pkg.org/badges/grand-total/retry)](https://cran.r-project.org/package=retry)

The retry package provides a simple mechanism to repeatedly evaluate an expression until either it succeeds or timeout exceeded. It is useful in situations that random failures could happen.

## Installation

You can install the released version of retry from [CRAN](https://CRAN.R-project.org) with:

```r
install.packages("retry")
```

The development version could be installed with:

```r
devtools::install_github("randy3k/retry")
```

## Example

We are going to use [future](https://github.com/HenrikBengtsson/future) to motivate some uses of `retry` and `wait_until`.
```r
library(retry)
library(future)
plan(multiprocess)

path <- tempfile()
f1 <- future({
    Sys.sleep(1)
    cat("hello\n", file = path)
})
retry(readLines(path), timeout = 5)
#> [1] "hello"


f2 <- future({
    Sys.sleep(1)
    cat("world\n", file = path, append = TRUE)
})
retry(readLines(path), until = ~ "world" %in% ., timeout = 5)
#> [1] "hello" "world"


f3 <- future({
    Sys.sleep(1)
    cat("hi\n", file = path, append = TRUE)
})
wait_until(resolved(f3))
readLines(path)
#> [1] "hello" "world", "hi"
```
