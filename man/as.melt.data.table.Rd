% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{as.melt.data.table}
\alias{as.melt.data.table}
\title{Fast melt to long data.table for large 2d matrix}
\usage{
as.melt.data.table(m, value_name = "value")
}
\arguments{
\item{m}{a 2d matrix}

\item{value_name}{text string for the data column, default is "value"}
}
\value{
a long data.table with row and col columns
}
\description{
Provides a fast method to melt a large 2D matrix into a long format data.table
this is particularly useful for converting gridded data into a format for using with ggplot2
}
\examples{
m = matrix(1:9, nrow = 3, ncol = 3)
as.melt.data.table(m) # long format data
}
