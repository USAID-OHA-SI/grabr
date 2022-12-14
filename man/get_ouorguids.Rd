% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_outable.R
\name{get_ouorguids}
\alias{get_ouorguids}
\title{Get Orgs uids by level}
\usage{
get_ouorguids(
  ouuid,
  level,
  username,
  password,
  baseurl = "https://final.datim.org/"
)
}
\arguments{
\item{ouuid}{Operatingunit uid}

\item{level}{Orgunit level}

\item{username}{Datim Account username}

\item{password}{Datim Account Password}

\item{baseurl}{Datim base url}
}
\value{
list of uids
}
\description{
Get Orgs uids by level
}
\examples{
\dontrun{
 library(grabr)

 # Set country of interest
 cntry <- "Zambia"

 # Get OU/Country orgunit uid
 uid <- get_ouuid(cntry)

 # Get org level for psnu
 lvl <- get_ouorglevel(cntry, org_type = "prioritization")

 # Retreived all uids for level 4 (SNU1)
 get_ouorguids(ouuid = uid, level = 4)
}

}
