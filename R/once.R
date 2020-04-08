#' Evaluate an expression once a condition is met.
#'
#' Repeatedly examine the given expression until it returns \code{TRUE}. Then trigger the evaluation
#' of another expression.
#'
#' @param expr the expression to check
#' @param then the expression to be execuated once
#' @param envir the environment in which the expressions are to be evaluated.
#' @param ... other parameters passed to \code{\link{wait_until}}.
#' @export
once <- function(expr, then, envir = parent.frame(), ...) {
    if (missing(then)) {
        abort("require \"then\" action")
    }
    expr <- enexpr(expr)
    then <- call2("withVisible", enexpr(then))
    wait_until(!!expr, envir = envir, ...)
    res <- eval_bare(then, envir)
    if (res$visible) {
        res$value
    } else {
        invisible(res$value)
    }
}
