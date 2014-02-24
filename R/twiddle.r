#' Overloaded tilde operator for in-place object modification.
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
#' This is dangerous meta-programming, but we live on the edge!
#' 
#' @param x anything. If \code{x} is an expression wrapped in braces,
#'   then this operator will assume it is a function call with the
#'   first argument intended for assignment of the resulting value.
#' @param y anything. Only used for compatibility with the formula operator.
#' @seealso \code{\link{~}}
#' @export
#' @examples
#' x <- c(1,2,3)
#' ~{factor(x))} # x is now a factor(c(1,2,3), levels = c(1,2,3))
#' 
#' x <- c(1,2,3)
#' ~{sum(x)} # x is now 6
`~` <- function(x, y = NULL) {
   if (is.call(tmp <- substitute(x)) && length(tmp) != 0 &&
       tmp[[1]] == as.symbol("{")) {
     tmp <- tmp[[2]]
     eval.parent(bquote(.(tmp[[2]]) <- .(tmp)))
   } else {
     foomula <-
       if (missing(y)) paste0("~", deparse(substitute(x)))
       else paste0(deparse(substitute(x)), " ~ ", deparse(substitute(y)))

     eval.parent(parse(text =
      paste0("(function() { `~` <- .Primitive(\"~\"); out <- ", foomula,
             "; environment(out) <- parent.frame(); out })()") ))
   }
}

