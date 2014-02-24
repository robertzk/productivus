#' In-place editing of expressions
#'
#' Edit an object in-place using the built-in \code{edit} function.
#'
#' One can choose the editor by passing an \code{editor} argument.
#' The default value for this is \code{getOption("editor")} as in
#' the \code{edit} function; possible values are "vi", "emacs",
#' "pico", "xemacs", "xedit", and potentially more (OS specific).
#'
#' @seealso \code{\link{edit}}
#' @param obj an object. It will be modified in place, i.e., calling
#'    \code{ed(obj)} is equivalent to \code{obj <- edit(obj)}.
#' @param ... additional arguments to pass to the built-in \code{edit}.
#' @export
#' @examples
#' some_function <- function(x) { x + 1}
#' ed(some_function) # opens an in-line editor to mess with some_function
ed <- function(obj, ...) eval.parent(substitute(obj <- edit(obj, ...)))

