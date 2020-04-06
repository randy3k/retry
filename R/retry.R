#' @docType package
#' @import rlang
#' @aliases NULL
"_PACKAGE"

#' Retry an expression
#'
#' Retry an expression until either timeout is exceeded or a condition is fullfilled.
#' @param expr an expression to be evaluated, quasiquotation is supported.
#' @param envir the environment in which the expression is to be evaluated.
#' @param upon a vector of condition classes. The expression will be retried if a
#' condition is thrown. See the \code{classes} parameter of \code{rlang::catch_cnd}.
#' @param until a function of two aruments. This function is used to check if we need to
#' retry \code{expr}. The first argument is the result of \code{expr} and the second argument
#' is the condition thrown when \code{expr} was evaluated. \code{retry} would return the
#' result of \code{expr} if \code{until} returns \code{TRUE}.
#' The default behavior is to retry unless no conditions are thrown.
#' It could be also a one sided formula that is later converted to a function
#' using \code{rlang::as_function}.
#' @param silent suppress messages and warnings
#' @param timeout raise an error if this amount of time in second has passed.
#' @param max_tries maximum number of attempts
#' @param interval delay between retries.
#' @param ... internal use only
#' @examples
#' retry(10)  # returns 10 immediately
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
                  upon = "error",
                  until = no_conditions_thrown,
                  silent = TRUE,
                  timeout = Inf,
                  max_tries = Inf,
                  interval = 0.1,
                  ...) {
    expr <- enexpr(expr)
    until <- as_function(until)
    ellipsis <- list(...)
    later_loaded <- isTRUE(ellipsis$later_run_now) && "later" %in% loadedNamespaces()

    t1 <- Sys.time()
    trial <- 0
    res <- NULL
    while (TRUE) {
        remaining <- t1 + timeout - Sys.time()
        if (remaining <= 0) {
            stop("timeout exceeded.")
        }
        once <- run_once(expr, envir, upon = upon, timeout = remaining, silent = silent)
        trial <- trial + 1
        res <- once$result
        cnd <- once$error
        if (later_loaded) {
            later::run_now()
        }
        if (isTRUE(until(res, cnd))) {
            if (!is.null(cnd)) {
                cnd_signal(cnd)
            }
            if (once$visible) {
                return(res)
            } else {
                return(invisible(res))
            }
        }
        if (trial >= max_tries) {
            stop("maximum number of tries exceeded.")
        }
        Sys.sleep(interval)
    }
}


no_conditions_thrown <- function(res, cnd) {
    is.null(cnd)
}


suppress <- function(expr, envir = parent.frame()) {
    expr <- enexpr(expr)
    suppressMessages(suppressWarnings(eval_bare(expr, envir)))
}


run_once <- function(expr, envir, upon, timeout, silent) {
    setTimeLimit(timeout, timeout)
    on.exit({
        setTimeLimit()
    })
    expr <- call2("withVisible", expr)

    if (silent) {
        cnd <- suppress(catch_cnd(res <- eval_bare(expr, envir), classes = upon))
    } else {
        cnd <- catch_cnd(res <- eval_bare(expr, envir), classes = upon)
    }

    if (is.null(cnd)) {
        return(
            list(result = res$value, visible = res$visible, error = NULL)
        )
    } else {
        return(
            list(result = NULL, visible = FALSE, error = cnd)
        )
    }
}
