% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pull_mer.R
\name{get_datim_targets}
\alias{get_datim_targets}
\title{DATIM API Call for Targets}
\usage{
get_datim_targets(url, username, password)
}
\arguments{
\item{url}{supply url forAPI call, recommend using`gen_url()`}

\item{username}{DATIM username}

\item{password}{DATIM password, recommend using `mypwd()`}
}
\description{
DATIM API Call for Targets
}
\examples{
\dontrun{
 myurl <- paste0(baseurl, "api/29/analytics.json?
                 dimension=LxhLO68FcXm:udCop657yzi&
                 dimension=ou:LEVEL-4;HfVjCurKxh2&
                 filter=pe:2018Oct&
                 displayProperty=SHORTNAME&outputIdScheme=CODE")
 myuser <- "UserX"
 df_targets <- get_datim_targets(myurl, myuser, mypwd(myuser)) }
}
