\name{ed}
\alias{ed}
\title{In-place editing of expressions}
\usage{
ed(obj = 1, tweak = NULL, verbose = FALSE, echo = getOption("ed.echo"),
  show.attributes = getOption("ed.show.attributes"), by = 0, to = NULL)
}
\arguments{
  \item{obj}{an object. It will be modified in place, i.e.,
  calling \code{ed(obj)} is equivalent to \code{obj <-
  edit(obj)}.  If an integer is passed, the last \code{obj}
  lines in the interactive console will be edited and
  executed. The default is \code{1}.}

  \item{tweak}{multiple. See \code{by} and \code{to}
  parameters.}

  \item{verbose}{logical. If \code{TRUE}, a copy of the
  edited value will be printed as a message.}

  \item{echo}{logical. Whether or not to edit the history
  in-place as well so that it shows up in the interactive
  command line.  The default is
  \code{getOption("ed.echo")}.}

  \item{show.attributes}{logical. Whether or not to show
  attributes when editing. The attributes will simply be
  copied over on editing if this is \code{FALSE}. The
  default is \code{getOption("ed.show.attributes")}}

  \item{by}{numeric. If \code{obj} is an integer, it will
  skip this many lines (i.e., it allows editing & executing
  *past* blocks in the history)}

  \item{to}{character. If \code{obj} is a character, edit
  all expressions the history starting with the latest
  matching \code{obj} up to the next latest matching
  \code{to}.}
}
\description{
Edit an object in-place using the built-in \code{edit}
function. If the first argument to \code{ed} is an integer,
it will attempt to edit the last \code{n} lines, excluding
references to \code{ed} or \code{lastn}.
}
\details{
One can choose the editor by passing an \code{editor}
argument. The default value for this is
\code{getOption("editor")} as in the \code{edit} function;
possible values are "vi", "emacs", "pico", "xemacs",
"xedit", and potentially more (OS specific).
}
\examples{
\dontrun{
some_function <- function(x) { x + 1 }
ed(some_function) # opens an in-line editor to mess with some_function

x <- 1:5
ed(x) # Will edit x in-place

ed(2) # Will edit a file containing
      #   some_function <_ function(x) { x + 1 }
      #   x <- 1:5
      # Editing and leaving the file will trigger its execution

x <- 10; attr(x, "y") <- 5
ed(x, v = T, e = T, s = T) # Will print a message containing the modified change
  # as well as echoing the results to the history, and showing attributes
  # in the editing pane.

ed("1:5", to = "10") # Edit the last two lines -- they are found through regular expressions
ed("1:5", "10")      # This actually works too :)
}
}
\seealso{
\code{\link{edit}}
}

