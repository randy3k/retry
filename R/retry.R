#' @docType package
#' @import rlang
#' @aliases NULL
"_PACKAGE"

#' Repeatedly evaluate an expression
#'
#' Repeatedly evaluate an expression until a condition is met or timeout is exceeded.
#'
#' @param expr an expression to be evaluated, supports quasiquotation.
#' @param upon a vector of condition classes. The expression will be evaluated again after
#' the delay if a condition is thrown. See the \code{classes} parameter of \code{rlang::catch_cnd}.
#' @param when regular expression pattern that matches the message of the condition. It is used to
#' decide if we need to evaluate \code{expr}.
#' @param until a function of two arguments. This function is used to check if we need to
#' evaluate \code{expr}. The first argument is the result of \code{expr} and the second argument
#' is the condition thrown when \code{expr} was evaluated.
#' It could be also a one sided formula that is later converted to a function
#' using \code{rlang::as_function}.
#' @param envir the environment in which the expression is to be evaluated.
#' @param silent suppress messages and warnings
#' @param timeout raise an error if this amount of time in seconds has passed.
#' @param max_tries maximum number of attempts
#' @param interval delay between retries.
#' @param later_run_now execute \code{later::run_now()} periodically when \code{later} is loaded?
#' @examples
#' retry(10, until = ~TRUE)  # returns immediately
#'
#' f <- function(x) {
#'     if (runif(1) < 0.9) {
#'         stop("random error")
#'     }
#'     x + 1
#' }
#' # keep retring when there is a random error
#' retry(f(1), when = "random error")
#' # keep retring until a condition is met
#' retry(f(1), until = function(val, cnd) val == 2)
#' # or using one sided formula
#' retry(f(1), until = ~ . == 2)
#'
#' try({
#'   # it doesn't capture the error of "a" + 1
#'   retry(f("a"), when = "random error")
#' })
#'
#' try({
#'   # an error is raised after 1 second
#'   retry(stop("foo"), when = "foo", timeout = 1)
#' })
#'
#' try({
#'   # timeout also works for indefinite R code
#'   retry(while(TRUE) {}, until = ~FALSE, timeout = 1)
#' })
#'
#' @export
retry <- function(expr,
                  upon = "error",
                  when = NULL,
                  until = NULL,
                  envir = parent.frame(),
                  silent = FALSE,
                  timeout = Inf,
                  max_tries = Inf,
                  interval = 0.1,
                  later_run_now = FALSE) {
    expr <- enexpr(expr)
    done <- done_factory(when, until)
    later_loaded <- isTRUE(later_run_now) && "later" %in% loadedNamespaces()

    t1 <- Sys.time()
    trial <- 0
    while (TRUE) {
        remaining <- t1 + timeout - Sys.time()
        if (remaining <= 0) {
            abort("timeout exceeded.")
        }
        result <- run_once(expr, envir, upon = upon, timeout = remaining, silent = silent)
        trial <- trial + 1
        value <- result$value
        cnd <- result$condition
        if (later_loaded) {
            later::run_now()
        }
        if (isTRUE(done(value, cnd))) {
            if (!is.null(cnd)) {
                cnd_signal(cnd)
            }
            if (result$visible) {
                return(value)
            } else {
                return(invisible(value))
            }
        }
        if (trial >= max_tries) {
            abort("maximum number of tries exceeded.")
        }
        Sys.sleep(interval)
    }
}


done_factory <- function(when, until) {
    if (is.null(when)) {
        if (is.null(until)) {
            abort("require at least one of the parameters \"when\" and \"until\".")
        }
        as_function(until)
    } else {
        if (is.null(until)) {
            function(val, cnd) {
                if (is.null(cnd) || !grepl(when, conditionMessage(cnd))) {
                    return(TRUE)
                }
            }
        } else {
            until <- as_function(until)
            function(val, cnd) {
                if (!is.null(cnd) && grepl(when, conditionMessage(cnd))) {
                    return(FALSE)
                }
                until(val, cnd)
            }
        }
    }
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
            list(value = res$value, visible = res$visible, condition = NULL)
        )
    } else {
        return(
            list(value = NULL, visible = FALSE, condition = cnd)
        )
    }
}
