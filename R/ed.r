#' In-place editing of expressions
#'
#' Edit an object in-place using the built-in \code{edit} function.
#' If the first argument to \code{ed} is an integer, it will attempt
#' to edit the last \code{n} lines, excluding references to \code{ed}
#' or \code{lastn}.
#'
#' One can choose the editor by passing an \code{editor} argument.
#' The default value for this is \code{getOption("editor")} as in
#' the \code{edit} function; possible values are "vi", "emacs",
#' "pico", "xemacs", "xedit", and potentially more (OS specific).
#'
#' @seealso \code{\link{edit}}
#' @param obj an object. It will be modified in place, i.e., calling
#'    \code{ed(obj)} is equivalent to \code{obj <- edit(obj)}.
#'    If an integer is passed, the last \code{obj} lines in the interactive
#'    console will be edited and executed. The default is \code{1}.
#' @param tweak multiple. See \code{by} and \code{to} parameters.
#' @param verbose logical. If \code{TRUE}, a copy of the edited
#'    value will be printed as a message.
#' @param echo logical. Whether or not to edit the history in-place
#'    as well so that it shows up in the interactive command line.
#'    The default is \code{getOption("ed.echo")}.
#' @param show.attributes logical. Whether or not to show attributes
#'    when editing. The attributes will simply be copied over on editing
#'    if this is \code{FALSE}. The default is \code{getOption("ed.show.attributes")}
#' @param by numeric. If \code{obj} is an integer, it will skip this many lines
#'    (i.e., it allows editing & executing *past* blocks in the history)
#' @param to character. If \code{obj} is a character, edit all expressions
#'    the history starting with the latest matching \code{obj} up to the
#'    next latest matching \code{to}.
#' @export
#' @examples
#' \dontrun{
#' some_function <- function(x) { x + 1 }
#' ed(some_function) # opens an in-line editor to mess with some_function
#'
#' x <- 1:5
#' ed(x) # Will edit x in-place
#'
#' ed(2) # Will edit a file containing
#'       #   some_function <_ function(x) { x + 1 }
#'       #   x <- 1:5
#'       # Editing and leaving the file will trigger its execution
#'
#' x <- 10; attr(x, "y") <- 5
#' ed(x, v = T, e = T, s = T) # Will print a message containing the modified change
#'   # as well as echoing the results to the history, and showing attributes
#'   # in the editing pane.
#' 
#' ed("1:5", to = "10") # Edit the last two lines -- they are found through regular expressions
#' ed("1:5", "10")      # This actually works too :)
#' }
ed <- function(obj = 1, tweak = NULL, verbose = FALSE, echo = getOption("ed.echo"),
               show.attributes = getOption("ed.show.attributes"),
               by = 0, to = NULL) {
  file <- NULL; replace <- FALSE; title <- NULL

  require(utils)
  stopifnot(interactive())

  # check unpromised values, since obj could be a variable containing an integer or character
  is_unpromised_integer <- grepl("^[0-9]+$", deparse(substitute(obj)))
  is_unpromised_character <- grepl('^".*"$', deparse(substitute(obj)))
  if (missing(to) && is_unpromised_character && (is.character(tweak) || is.integer(tweak))) {
    # If tweak is a character or integer, we are missing the "to" parameter, and
    # the obj parameter is a character, make ed("foo", "bar") a shortcut for
    # ed("foo", to = "bar")
    to <- tweak
  }

  if (missing(by) && is_unpromised_integer && (is.character(tweak) || is.integer(tweak))) {
    # If tweak is a character or integer, we are missing the "by" parameter, and
    # the obj parameter is an integer, make ed(5, 3) a shortcut for
    # ed(5, by = 3)
    by <- tweak
  }

  safe_file <- function(string) str_replace_all(obj, "[()\\^$.|?*\\[\"']", "")
  # If a number, edit and execute the last n lines
  expr <- if (is_unpromised_integer) { # don't evaluate promises
    title <- paste0("Editing previous ", obj, " lines")
    file <- tempfile(paste0(safe_file(title), "                "))
    writeLines(str_split(lastn(obj + by, eval = FALSE), "\n")[[1]][seq_len(obj)], file)
  } else if (is_unpromised_character) { # don't evaluate promises
    title <- paste0("Editing line matching '", obj, "'")
    if (is.character(to)) title <- paste0(c(title, " through '", to, "'"), collapse = "")
    lines <- find_lines_in_history(obj, to)
    file <- tempfile(paste0(safe_file(title), "                "))
    writeLines(lines, file)
  } else { 
    title <- paste0("Editing ", deparse(substitute(obj)))
    file <- tempfile(paste0(title, "               "))
    control <- eval(formals(dput)$control)
    if (!identical(show.attributes, TRUE))
      control <- setdiff(control, 'showAttributes')
    # Store the object text representation for editing
    dput(obj, file, control = control)
    replace <- TRUE
  }

  modtime <- file.info(file)$mtime
  output <- file.edit(file, title = title)
  if (file.info(file)$mtime == modtime)
    return(invisible(NULL)) # File was closed with no changes


  # If we're hiding attributes, do a simpler assignment now and copy the 
  # attributes over later
  pre_raw_text <- paste0(readLines(file), collapse = "\n")
  raw_text <-
    if (replace) paste(deparse(substitute(obj)), "<- {", pre_raw_text, "}")
    else pre_raw_text

  # Now is the time to print the message!
  if (identical(verbose, TRUE)) message(raw_text)

  if (identical(echo, TRUE)) {
    # A sneaky fun trick. cat out the lines executed to make it seem like
    # they are being populated in the command line history -- which they are!
    .prompt <- getOption("prompt")
    decorated_lines <- paste0(vapply(strsplit(raw_text, "\n")[[1]],
      function(line) paste0(.prompt, line), character(1)), collapse = "\n")
    cat(decorated_lines, "\n")

    # Now save the executed commands to the history so the user can access them
    # by, e.g., pressing the up arrow.
    tmpfile <- tempfile()
    savehistory(tmpfile)
    tmpfilea <- file(file.path(tmpfile), 'a') # open an append connection
    write(raw_text, tmpfilea)
    close(tmpfilea)
    loadhistory(tmpfile) # Append the commands to the current history
    unlink(tmpfile)
  }

  if (replace && !identical(show.attributes, TRUE)) {
    # If we're replacing the object in-place, keep the old attributes as well,
    # but since this is messy, let's hide it from the user if they specify
    # verbose = TRUE. If they have chosen to show the attributes, don't do this
    # since they've already edited them themselves.
    obj_name <- deparse(substitute(obj))
    raw_text <- paste("`*xtmp*` <- attributes(", obj_name, ");",
                      obj_name, "<- {", pre_raw_text, "};",
                      "attributes(", obj_name, ") <- `*xtmp*`; rm(`*xtmp*`);",
                      obj_name)
  }

  invisible(eval.parent(parse(text = raw_text)))
}

