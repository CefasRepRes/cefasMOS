% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo.R
\name{smartbuoy.map}
\alias{smartbuoy.map}
\title{Deployment Map}
\usage{
smartbuoy.map(
  platforms = c(1, 4, 8),
  deployment_group_id = "ALL",
  style = "gsat",
  point_size = 5,
  labels = TRUE,
  active_only = TRUE,
  zoom_to_group = TRUE,
  db_name = "smartbuoydblive"
)
}
\arguments{
\item{platforms}{optional integer vector, if supplied only these platforms will be displayed. see details. default is c(1, 4, 8)}

\item{deployment_group_id}{optional chararacter vector, if supplied only these deployments will be displayed.}

\item{style}{character string denoting map style, default is 'gsat'}

\item{point_size}{numeric, size of plotted points}

\item{labels}{boolean, if True labels are plotted using directlabels.}

\item{active_only}{boolean, if True only current active deployments will be displayed}

\item{zoom_to_group}{boolean, if True map is centred and zoomed to selected deployments, if False entire UK is used.}

\item{db_name}{character string matching ODBC data source name, defaults to 'smartbuoydblive'}
}
\value{
ggmap object
}
\description{
Creates a map of MOS deployment sites
}
\details{
1 for SmartBuoy, 4 = Lander, 8 = Waverider
}
\keyword{SmartBuoy}
