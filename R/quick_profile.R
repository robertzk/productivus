#' Very simple profiling of R code.
#'
#' @param expr expression. An R expression to evaluate and profile.
#' @return The output of \code{\link[utils]{summaryRprof}} on that expression.
#' @export
#' @examples
#' quick_profile({
#'   Sys.sleep(1)
#'   cat("That took a while!")
#' })
quick_profile <- function(expr) {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  Rprof(tmp, interval = 0.001)
  eval(substitute(expr), envir = parent.frame())
  Rprof(NULL)
  summaryRprof(tmp)
}

