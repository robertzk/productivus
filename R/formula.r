#' Overloaded formula operator for in-place object modification.
#'
#' This function overloads the \code{~} operator to turn statements like
#'   \code{~\{factor(x, levels = c("A", "B"))\}}
#' into
#'   \code{x <- factor(x, levels = c("A", "B")}
#' while leaving normal formula expressions alone.
#'
#' In general, if we called \code{~\{someexpr(x, ...)\}}, turn that into
#'   \code{x <- someexpr(x, ...)}
#' in the parent environment. Otherwise, treat it as a formula.
#' 
#' @param x anything. If \code{x} is an expression wrapped in braces,
#'   then this operator will assume it is a function call with the
#'   first argument intended for assignment of the resulting value.
#' @param y anything. Only used for compatibility with the formula operator.
#' @seealso \link{\code{base::`~`}}
#' @export
`~` <- function(x, y = NULL) {
   if (is.call(tmp <- substitute(x)) && length(tmp) != 0 &&
       tmp[[1]] == as.symbol("{")) {
     tmp <- tmp[[2]]
     eval.parent(bquote(.(tmp[[2]]) <- .(tmp)))
   } else {
     eval.parent(parse(text =
      paste0("(function() { `~` <- .Primitive(\"~\"); out <- ",
             deparse(substitute(x)), " ~ ", deparse(substitute(y)), "; ",
             "environment(out) <- parent.frame(); out })()") ))
   }
}

