
# retry

<!-- [![Github
Action](https://github.com/randy3k/retry/workflows/build/badge.svg?branch=master)](https://github.com/randy3k/retry) -->
<!-- [![codecov](https://codecov.io/gh/randy3k/retry/branch/master/graph/badge.svg)](https://codecov.io/gh/randy3k/retry) -->

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/retry)](https://cran.r-project.org/package=retry)
[![](http://cranlogs.r-pkg.org/badges/grand-total/retry)](https://cran.r-project.org/package=retry)

The retry package provide an easy mechanism to keep evaluating an expression until either it successes or timeout. It is useful in situations that random failures could happen, examples would be network or file IO.

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


```r
library(retry)

path <- tempfile()
callr::r_bg(function(path) cat("hello\n", file = path), list(path = path))
#> PROCESS 'R', running, pid 86861.
retry(readLines(path), timeout = 5, silent = FALSE)
#> [1] "hello"

callr::r_bg(function(path) cat("world\n", file = path, append = TRUE), list(path = path))
#> PROCESS 'R', running, pid 87127.
retry(readLines(path), until = ~ "world" %in% ., timeout = 5)    
#> [1] "hello" "world"
```
