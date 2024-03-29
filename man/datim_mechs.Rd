% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_datim.R
\name{datim_mechs}
\alias{datim_mechs}
\title{Extract Mechanisms infos from Datim}
\usage{
datim_mechs(cntry, username, password, agency = "USAID", baseurl = NULL)
}
\arguments{
\item{cntry}{Country name}

\item{username}{Datim username}

\item{password}{Datim password}

\item{agency}{Agency name}

\item{baseurl}{Datim API Base URL}
}
\value{
OU Mechanisms as data frame
}
\description{
Extract Mechanisms infos from Datim
}
\examples{
\dontrun{
  library(grabr)

  datim_mechs(
   cntry = "Mozambique",
   username = glamr::datim_user(),
   password = glamr::datim_pwd()
  )
}
}
