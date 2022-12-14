% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_datim.R
\name{datim_dimensions}
\alias{datim_dimensions}
\title{Get PEPFAR/DATIM dimensions}
\usage{
datim_dimensions(
  url = "https://final.datim.org/api/dimensions",
  username,
  password,
  var = NULL
)
}
\arguments{
\item{url}{DATIM API End point}

\item{username}{DATIM Account Username}

\item{password}{DATIM Account passward}

\item{var}{Column name to pull all values from, default is NULL, options are: id, dimension}
}
\value{
Dimensions as tibble or list of ids / dimension names
}
\description{
Get PEPFAR/DATIM dimensions
}
\examples{
\dontrun{
  library(grabr)

  datim_dimensions()
}

}
