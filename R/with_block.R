#' Turn a function into one that accepts a block, ruby-style.
#'
#' @param fn function. The function to convert into a block-accepting function.
#'    When calling \code{yield()} from within the function, the block will be
#'    substituted instead.
#' @return a function with one additional argument called \code{"_block"} 
#'    that will store an expression to evaluate using \code{yield()} from
#'    within the body of \code{fn}. Additional "arguments" can be passed
#'    to \code{yield} which will be injected into the lexical scope of
#'    the evaluation.
#' @export
#' @examples
#' blocked_fn <- with_block(function(x, y) x + y + yield())
#' stopifnot(identical(blocked_fn(1, 2, { 3 + 4 }), 10))
#' 
#' assign_names <- with_block(function(x) {
#'  setNames(x, vapply(x, function(y) paste("element_", yield(name = y)), character(1)))
#' })
#' stopifnot(identical(assign_names(letters[1:5], { toupper(name) }),
#'   list(A = 'a', B = 'b', C = 'c', D = 'd', E = 'e')))
with_block <- function(fn) {
  # TODO: Handle splats  
  stopifnot(is.function(fn))

  fn <- add_block_to_formals(fn)


}

add_block_to_formals <- function(fn) {
  formals <- formals(fn)
  formals[length(formals) + 1] <- alist(dummy = )
  names(formals)[length(formals)] <- "_block"
  formals(fn) <- formals

  fn
}

