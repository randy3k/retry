
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

Some examples of `retry` and `wait_until`.
```r
library(retry)

f <- function(x) {
    if (runif(1) < 0.9) {
        stop("random error")
    }
    x + 1
}

# keep retring when there is a random error
retry(f(1), when = "random error")
#> [1] 2
# keep retring until a condition is met
retry(f(1), until = ~ . == 2)
#> [1] 2
# or using one sided formula
retry(f(1), until = ~ . == 2)
#> [1] 2

z <- 0
later::later(function() z <<- 1, 1)
wait_until(z == 1)
z
#> [1] 1
```
