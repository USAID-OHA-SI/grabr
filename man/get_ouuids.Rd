% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_outable.R
\name{get_ouuids}
\alias{get_ouuids}
\title{Get OU Org UIDS}
\usage{
get_ouuids(
  add_details = FALSE,
  username,
  password,
  baseurl = "https://final.datim.org/"
)
}
\arguments{
\item{add_details}{Add countries for regional ou, default is false}

\item{username}{DATIM Username, recommend using glamr::datim_user()`}

\item{password}{DATIM password, recommend using glamr::datim_pwd()`}

\item{baseurl}{base url for the API, default = https://final.datim.org/}
}
\value{
OU UIDS as tibble
}
\description{
Get OU Org UIDS
}
\examples{
\dontrun{
 library(grabr)

 # OU Org UIDs
 ous <- get_ouuids()
}

}
