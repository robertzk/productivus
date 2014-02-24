\name{lastn}
\alias{last1,}
\alias{last2,}
\alias{last3,}
\alias{last4,}
\alias{last5,}
\alias{last6,}
\alias{last7,}
\alias{last8,}
\alias{last9}
\alias{lastn}
\title{Re-execute the last n lines in the interactive console.}
\usage{
lastn(n, verbose = FALSE, eval = TRUE)
}
\arguments{
  \item{n}{numeric. The last \code{n} lines to execute.}

  \item{verbose}{logical. Whether or not to display the
  lines getting executed as a message. Default is
  \code{FALSE} (i.e., do not display).}

  \item{eval}{logical. Whether to run the string of code
  that will be executed, or merely return the character
  containing its contents.}
}
\description{
This function will ignore any instances of \code{lastn}
itself, so that running \code{lastn(2)} after x <- 1
lastn(1) x <- x + 1 will result in \code{x = 2}, not
\code{x = 4} (because \code{x} gets re-initialized to
\code{1}).
}
\details{
There are also helper functions \code{last1}, \code{last2},
... \code{last9} which call this function with \code{n = 1,
2, ... 9} respectively.

Note: This function will also ignore past \code{ed}
commands.
}

