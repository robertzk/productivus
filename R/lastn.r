#' Re-execute the last n lines in the interactive console.
#'
#' This function will ignore any instances of \code{lastn} itself,
#' so that running \code{lastn(2)} after
#'    x <- 1
#'    lastn(1)
#'    x <- x + 1
#' will result in \code{x = 2}, not \code{x = 4} (because \code{x}
#' gets re-initialized to \code{1}).
#'
#' There are also helper functions \code{last1}, \code{last2}, ...
#' \code{last9} which call this function with \code{n = 1, 2, ... 9}
#' respectively.
#'
#' Note: This function will also ignore past \code{ed} commands.
#'
#' @param n numeric. The last \code{n} lines to execute.
#' @param verbose logical. Whether or not to display the lines getting
#'    executed as a message. Default is \code{FALSE} (i.e., do not display).
#' @param eval logical. Whether to run the string of code that will be
#'    executed, or merely return the character containing its contents.
#' @aliases last1, last2, last3, last4, last5, last6, last7, last8, last9
#' @export
lastn <- function(n, verbose = FALSE, eval = TRUE) {
  rawhist <- raw_history()
  nlines <- length(rawhist)
  inds <- max(1, nlines - n + 1):nlines
  runlines <- paste0(rawhist[inds], collapse = "\n")
  if (verbose) message(runlines)

  if (eval) eval.parent(parse(text = runlines))
  else runlines
}

#' @export
last1 <- function(...) eval.parent(substitute(lastn(1, ...)))
#' @export
last2 <- function(...) eval.parent(substitute(lastn(2, ...)))
#' @export
last3 <- function(...) eval.parent(substitute(lastn(3, ...)))
#' @export
last4 <- function(...) eval.parent(substitute(lastn(4, ...)))
#' @export
last5 <- function(...) eval.parent(substitute(lastn(5, ...)))
#' @export
last6 <- function(...) eval.parent(substitute(lastn(6, ...)))
#' @export
last7 <- function(...) eval.parent(substitute(lastn(7, ...)))
#' @export
last8 <- function(...) eval.parent(substitute(lastn(8, ...)))
#' @export
last9 <- function(...) eval.parent(substitute(lastn(9, ...)))
