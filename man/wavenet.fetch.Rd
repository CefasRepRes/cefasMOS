% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wavenet.R
\name{wavenet.fetch}
\alias{wavenet.fetch}
\title{WaveNet data query tool}
\usage{
wavenet.fetch(
  deployment = NA,
  deployment_group = NA,
  parameters = c("Hm0", "Tpeak", "Tz", "W_PDIR", "W_SPR"),
  after = NA,
  before = NA,
  telemetry = F,
  db_name = "smartbuoydblive"
)
}
\arguments{
\item{deployment}{optional string matching smartbuoy deployment}

\item{deployment_group}{optional string matching smartbuoy deployment_group}

\item{after}{optional date string, if provided only data after this date will be returned, assumes UTC e.g. "2014-08-10"}

\item{before}{optional date string, if provided only data before this date will be returned, assumes UTC e.g. "2014-12-09"}

\item{telemetry}{If true then query the telemetry table.}

\item{db_name}{character string matching ODBC data source name, defaults to 'smartbuoydblive'}
}
\value{
data.table with returned data in "long" format or error string if no data returned
}
\description{
Fetches WaveNet DWR data
}
\details{
This function queries the SmartBuoy database and returns a data.table
}
\keyword{query}
\keyword{wavenet}
