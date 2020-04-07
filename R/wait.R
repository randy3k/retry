#' Wait until a condition is met
#'
#' Block the current runtime until the expression returns \code{TRUE}.
#'
#' @param expr an expression to check, supports quasiquotation.
#' @param envir the environment in which the expression is to be evaluated.
#' @param timeout raise an error if this amount of time in second has passed.
#' @param interval delay between retries.
#' @param later_run_now execute \code{later::run_now()} periodically \code{later} is loaded?
#' @examples
#'
#' s <- Sys.time()
#' system.time(wait_until(Sys.time() - s > 1))
#'
#' z <- 0
#' later::later(function() z <<- 1, 1)
#' wait_until(z == 1)
#' z == 1
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
