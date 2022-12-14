% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_general.R
\name{pipe_cond}
\alias{pipe_cond}
\title{<data frame> use if else condition with pipe operator.}
\usage{
pipe_cond(.data, condition, .true, .false = identity)
}
\arguments{
\item{.data}{a data frame.}

\item{condition}{a condition that returns a Boolean.}

\item{.true}{the value to return if the condition is true.}

\item{.false}{the value to return if the condition is false.}
}
\value{
a tibble.
}
\description{
<data frame> use if else condition with pipe operator.
}
\examples{
pipe_cond(data, max(profit) > 1000, ~group_by(.x, sale_channel), ~.x)

}
