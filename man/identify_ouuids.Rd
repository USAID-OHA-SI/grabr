% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_outable.R
\name{identify_ouuids}
\alias{identify_ouuids}
\title{Pull OU UIDS}
\usage{
identify_ouuids(username, password, baseurl = "https://final.datim.org/")
}
\arguments{
\item{username}{DATIM Username, defaults to using glamr::datim_user()` if blank}

\item{password}{DATIM password, defaults to using glamr::datim_pwd()` if blank}

\item{baseurl}{base url for the API, default = https://final.datim.org/}
}
\value{
Datim country names
}
\description{
`identify_ouuids` pulls from DATIM to return a dataframe with all PEPFAR
Operating Units and countries and their UIDs. This is one of two components
that feeds into `get_outable`.
}
\details{
To access the UIDs, the user will need to have a DATIM account. You can
take advantage of storing you credentials locally in a secure way
using `set_datim`.
}
\examples{
\dontrun{
 load_secrets()
 ous <- identify_ouuids() }
}
\seealso{
[set_datim()] to store DATIM authentication;
  [load_secrets()] to load credentials into session
}
