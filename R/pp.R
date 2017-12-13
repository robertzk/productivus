## TODO: more extensive documentation
#' Ruby-style string interpolation
#'
#' Writing strings for messages with variable content often involves a mess of
#' nested calls to \code{paste}. Use this function to reduce the clutter created by these.
#'
#' @examples
#' \dontrun{
#'   x <- 5
#'   pp("we assigned ${x} to x")
#'   x <- "this time we'll use a string"
#'   pp("we assigned ${x} to x, and the value of x changed")
#'   .small_env <- new.env()
#'   .small_env$x <- "this is inside an environment"
#'   pp("if we declare an environment, we see that x is ${x} there", envir = .small_env)
#' }
#' @param ... a character vector, or a (possibly nested) list of character vectors.
#' @param envir environment. The environment to fetch values to use in interpolation.
#' @param sep character. Passed into the \code{collapse} argument of \code{base::paste}.
#' @param collapse character. Passed into the \code{collapse} argument of \code{base::paste}.
#' @export
pp <- function(..., envir = parent.frame(), sep = '', collapse = '') {
  string <- list(...)
  if (length(string) > 1)
    return(paste(sapply(string,
      function(s) { pp(s, envir = envir, sep = sep, collapse = collapse) }
    ), collapse = sep))
  string <- string[[1]]
  if (length(string) > 1)
    return(paste(sapply(string,
      function(s) { pp(s, envir = envir, sep = sep, collapse = collapse) }
    ), collapse = collapse))
  regex <- gregexpr('#\\{([^\\}]+)\\}', string, perl=TRUE)
  starts <- attr(regex[[1]], 'capture.start')
  lengths <- attr(regex[[1]], 'capture.length')
  buildstr <- ''
  last <- 1
  for(i in 1:length(attr(regex[[1]], 'capture.start'))) {
    buildstr <- append(buildstr,
      c(substr(string, last, starts[i] - 3),
        eval(parse(text = substr(string, starts[i], starts[i] + lengths[i] - 1)),
        envir = envir)
       ))

    last <- starts[i] + lengths[i] + 1
  }
  buildstr <- append(buildstr, substr(string, last, nchar(string)))
  paste(buildstr, collapse = '')
}


#' @export
pmessage <- function(...) { message(pp(...)) }

#' @export
pwarning <- function(...) { warning(pp(...)) }

#' @export
pstop <- function(...) { stop(pp(...)) }

