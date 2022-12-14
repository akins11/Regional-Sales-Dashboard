% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_general.R
\name{rtbl_sub_pc_summary}
\alias{rtbl_sub_pc_summary}
\title{<table> showing the summary of quantity order by products or customer.}
\usage{
rtbl_sub_pc_summary(f_df, count_var, s_fun = "sum")
}
\arguments{
\item{f_df}{data frame or a reactive data frame.}

\item{count_var}{the character variable to count either product or customer name.}

\item{s_fun}{aggregate function.}
}
\value{
react table output.
}
\description{
<table> showing the summary of quantity order by products or customer.
}
\examples{
rtbl_sub_pc_summary(r_df(), "customer_names")

}
