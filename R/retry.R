#' Retry an expression
#'
#' Retry an expression until either timeout is exceeded or a condition is fullfilled.
#' @param expr an expression to be evaluated, quasiquotation is supported.
#' @param envir the environment in which the expression is to be evaluated.
#' @param until a function of two aruments. This function is used to check if we need to
#' re-evaluate \code{expr}. The first argument is the result of \code{expr} and the second argument
#' is the potential error condition when \code{expr} is evaluated. The `retry` function returns the
#' result of \code{expr} if \code{until} returns \code{TRUE}.
#' Default value is \code{function(res, cnd) \{is.null(cnd)\}}.
#' It could be also a one sided formula that is later converted to a function
#' using \code{rlang::as_function}.
#' @param silent suppress messages and warnings
#' @param timeout raise an error if this amount of time in second has passed.
#' @param interval delay between retries.
#' @examples
#' retry(10)  # returns 10 imediately
#'
#' elapse <- function(s, by) {
#'   t <- Sys.time()
#'   if (t - s < by) {
#'     stop()
#'   }
#'   return(t - s)
#' }
#' s <- Sys.time()
#' retry(elapse(s, 1))  # retry until success
#'
#' \dontrun{
#'   # this won't work because each retry reruns Sys.time()
#'   retry(elapse(Sys.time(), 1), timeout = 3)
#' }
#'
#' # instead, we could use quasiquotation
#' retry(elapse(!!(Sys.time()), 1))
#'
#' x <- 0
#' counter <- function() {
#'   x <<- x + 1
#'   x
#' }
#' retry(counter(), until = function(res, cnd) res == 10)
#'
#' x <- 0
#' retry(counter(), until = ~ . == 10)
#'
#' \dontrun{
#'   # an error is raised after 1 second
#'   retry(stop(), timeout = 1)
#'
#'   # timeout also works for indefinite R code
#'   retry(while(TRUE) {}, timeout = 1)
#' }
#' @export
retry <- function(expr,
                  envir = parent.frame(),
                  until = .default_until,
                  silent = TRUE,
                  timeout = Inf,
                  interval = 0.1) {
    expr <- rlang::enexpr(expr)
    until <- rlang::as_function(until)

    t1 <- Sys.time()
    res <- NULL
    while (TRUE) {
        remaining <- t1 + timeout - Sys.time()
        if (remaining <= 0) {
            stop("timeout exceeded.")
        }
        once <- run_once(expr, envir, remaining, silent)
        res <- once$result
        cnd <- once$error
        if (isTRUE(until(res, cnd))) {
            if (!is.null(cnd)) {
                rlang::cnd_signal(cnd)
            }
            if (once$visible) {
                return(res)
            } else {
                return(invisible(res))
            }
        }
        Sys.sleep(interval)
    }
}


#' Block the current runtime until the expression returns \code{TRUE}.
#' @param expr the expression to check
#' @param envir the environment in which the expression is to be evaluated.
#' @param timeout raise an error if this amount of time in second has passed.
#' @param interval delay between retries.
#' @examples
#'
#' s <- Sys.time()
#' system.time(wait_until(Sys.time() - s > 1))
#'
#' @export
wait_until <- function(expr, envir = parent.frame(), timeout = Inf, interval = 0.1) {
    expr <- rlang::enexpr(expr)
    retry(
        invisible(NULL),
        until = function(res, cnd) eval(expr, envir),
        timeout = timeout,
        interval = interval)
}


.default_until <- function(res, cnd) {
    is.null(cnd)
}


run_once <- function(expr, envir, timeout, silent) {
    is_error <- FALSE
    warn_cnd <- NULL
    msg_cnd <- NULL
    err_handler <- function(e) {
        is_error <<- TRUE
        e
    }
    setTimeLimit(timeout, timeout)
    on.exit({
        setTimeLimit()
    })
    expr <- rlang::call2("withVisible", expr)
    if (silent) {
        res <- suppressMessages(
            suppressWarnings(
                tryCatch(eval(expr, envir), error = err_handler)
            )
        )
    } else {
        res <- tryCatch(eval(expr, envir), error = err_handler)
    }
    if (is_error) {
        return(
            list(result = NULL, visible = FALSE, error = res)
        )
    } else {
        return(
            list(result = res$value, visible = res$visible, error = NULL)
        )
    }
}
