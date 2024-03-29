% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sugar.R
\name{orn}
\alias{orn}
\title{Syntactic Sugar for Run Construction}
\usage{
orn(id, parquet = parquet_default(), test_server = test_server_default())
}
\arguments{
\item{id}{(\code{integer(1)})\cr
OpenML id for the object.}

\item{parquet}{(\code{logical(1)})\cr
Whether to use parquet instead of arff.
If parquet is not available, it will fall back to arff.
Defaults to value of option \code{"mlr3oml.parquet"} or \code{FALSE} if not set.}

\item{test_server}{(\code{character(1)})\cr
Whether to use the OpenML test server or public server.
Defaults to value of option \code{"mlr3oml.test_server"}, or \code{FALSE} if not set.}
}
\value{
(\code{\link{OMLRun}})
}
\description{
Creates an \code{\link{OMLRun}} instance.
}
