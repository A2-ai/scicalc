% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_file_with_hash.R
\name{read_xpt_with_hash}
\alias{read_xpt_with_hash}
\title{Reads data from xpt file and prints hash of contents.}
\usage{
read_xpt_with_hash(xpt_file_path, ...)
}
\arguments{
\item{xpt_file_path}{an xpt file to ingest}

\item{...}{additional arguments to digest or read_xpt}
}
\value{
a dataframe(?) of data within file
}
\description{
Reads data from xpt file and prints hash of contents.
}
\examples{
\dontrun{
read_xpt_with_hash("data/source/example.xpt")
}
}
