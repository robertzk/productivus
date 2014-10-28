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
#'  setNames(x, vapply(x, function(y) paste0("element_", yield(name = y)), character(1)))
#' })
#' stopifnot(identical(assign_names(letters[1:5], { toupper(name) }),
#'   list(element_A = 'a', element_B = 'b', element_C = 'c',
#'     element_D = 'd', element_E = 'e')))
#'
#' maybe_block <- with_block(function() { list(1, if (block_given()) yield()) })
#' stopifnot(identical(maybe_block(2), list(1, 2)) && identical(maybe_block(), list(1)))
with_block <- function(fn) {
  # TODO: Handle splats  
  stopifnot(is.function(fn))

  fn <- add_block_to_formals(fn)
  fn <- inject_yield(fn)

  fn
}

add_block_to_formals <- function(fn) {
  formals <- formals(fn)
  formals[length(formals) + 1] <- alist(dummy = )
  names(formals)[length(formals)] <- "_block"
  formals(fn) <- formals

  fn
}

inject_yield <- function(fn) {
  injection <- new.env(parent = environment(fn))
  injection$yield <- function(...) {
    arguments <- list(...)
    # Work backwards through call stack in case of nested calls.
    # TODO: (RK) Ever need to support nested blocked function calls?
    expr <- NULL
    for (i in seq_along(sys.frames())) {
      if (exists('_block', where = parent.frame(i), inherits = FALSE)) {
        expr <- substitute(`_block`, parent.frame(i))
        break
      }
    }
    if (is.null(expr)) stop("Could not find block - did you call yield() correctly?")

    if (length(arguments) == 0) {
      eval(expr, envir = parent.frame(2))
    } else {
      tmp <- as.environment(arguments)
      parent.env(tmp) <- parent.frame(2)
      eval(expr, envir = tmp)
    }
  }
  injection$block_given <- function() {
    eval.parent(quote(!missing(`_block`)))
  }
  environment(fn) <- injection
  fn
}

