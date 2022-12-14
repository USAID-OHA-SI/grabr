% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_s3objects.R
\name{s3_download}
\alias{s3_download}
\title{Download S3 Objects}
\usage{
s3_download(bucket, object, filepath = NULL, access_key, secret_key, ...)
}
\arguments{
\item{bucket}{S3 Bucket name}

\item{object}{S3 Object key (id)}

\item{filepath}{Full path of destination file}

\item{access_key}{S3 Access key id}

\item{secret_key}{S3 Secret Access key}

\item{...}{Additional aws.S3::save_object() options}
}
\value{
file name
}
\description{
Download S3 Objects
}
\examples{
\dontrun{
s3_objects("sample-bucket") \%>\%
  filter(str_detect(key, "^HFR")) \%>\%
  pull(key) \%>\%
  first() \%>\%
  s3_download()}

}
