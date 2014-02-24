\name{~}
\alias{~}
\title{Overloaded formula operator for in-place object modification.}
\usage{
"~"(x, y = NULL)
}
\arguments{
  \item{x}{anything. If \code{x} is an expression wrapped
  in braces, then this operator will assume it is a
  function call with the first argument intended for
  assignment of the resulting value.}

  \item{y}{anything. Only used for compatibility with the
  formula operator.}
}
\description{
This function overloads the \code{~} operator to turn
statements like \code{~\{factor(x, levels = c("A", "B"))\}}
into \code{x <- factor(x, levels = c("A", "B")} while
leaving normal formula expressions alone.
}
\details{
In general, if we called \code{~\{someexpr(x, ...)\}}, turn
that into \code{x <- someexpr(x, ...)} in the parent
environment. Otherwise, treat it as a formula.
}
\seealso{
\link{\code{base::`~`}}
}

