#' Block the current runtime until the expression returns \code{TRUE}.
#' @param expr the expression to check
#' @param envir the environment in which the expression is to be evaluated.
#' @param timeout raise an error if this amount of time in second has passed.
#' @param interval delay between retries.
#' @param later_run_now execute \code{later::run_now()} periodically?
#' @examples
#'
#' s <- Sys.time()
#' system.time(wait_until(Sys.time() - s > 1))
#'
#' @export
wait_until <- function(expr,
                       envir = parent.frame(),
                       timeout = Inf,
                       interval = 0.1,
                       later_run_now = TRUE) {
    expr <- rlang::enexpr(expr)
    retry(
        invisible(NULL),
        until = function(res, cnd) eval_bare(expr, envir),
        timeout = timeout,
        interval = interval,
        later_run_now = later_run_now
    )
}
