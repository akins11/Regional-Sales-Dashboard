% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_general.R
\name{filter_variable}
\alias{filter_variable}
\title{<data frame> filter a variable based on a category.}
\usage{
filter_variable(df, variable, category)
}
\arguments{
\item{df}{data frame.}

\item{variable}{the variable to filter.}

\item{category}{a sunset of the variable to keep.}
}
\value{
a tibble.
}
\description{
<data frame> filter a variable based on a category.
}
\examples{
filter_variable(df, "customer_names", "Ayomide Samuel")

}
