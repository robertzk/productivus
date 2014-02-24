#' @param n numeric. The last \code{n} lines to execute.
#' @param verbose logical. Whether or not to display the lines getting
#'    executed as a message. Default is \code{FALSE} (i.e., do not display).
#' @aliases last1, last2, last3, last4, last5, last6, last7, last8, last9
#' @export
lastn <- function(n, verbose = FALSE) {
  file1 <- tempfile("Rrawhist")
  savehistory(file1)
  rawhist <- readLines(file1)
  rawhist <- rawhist[setdiff(seq_len(length(rawhist)),
                             grep("^\\s*last[1-9n]\\(.*\\)\\s*", rawhist))]
  unlink(file1)
  nlines <- length(rawhist)
  inds <- max(1, nlines - n + 1):nlines
  runlines <- paste0(rawhist[inds], collapse = "\n")
  if (verbose) message(runlines)
  eval.parent(parse(text = runlines))
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
