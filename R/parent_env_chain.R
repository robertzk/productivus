#' The parent environment chain of a given environment.
#'
#' @param envir environment. By default, the \code{parent.frame()}.
#' @return a list of parent environments, with the first element being
#'    the immediate parent, and the last element being the empty environment.
#' @note If a parent environment terminates with an environment who is its
#'    own parent, this will be the last element instead for the returned list.
parent_env_chain <- function(envir = parent.frame()) {
  envs <- list()
  while (!identical(envir, emptyenv()) && !identical(envir, parent.env(envir))) {
    envs[[length(envs) + 1]] <- envir
    envir <- parent.env(envir)
  }
  envs
}
