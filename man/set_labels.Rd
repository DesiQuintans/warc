% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zz_deprecated.R
\name{set_labels}
\alias{set_labels}
\title{Deprecated: Set labels on many dataframe columns at once}
\usage{
set_labels(df, ...)
}
\arguments{
\item{df}{(Dateframe) The dataframe.}

\item{...}{(dots) Column names and labels in the form \code{colname = "label"}.}
}
\value{
Nothing. Changes the dataframe in-place.
}
\description{
So that we can work with very large dataframes, this function modifies the
dataframe in-place (i.e. has the side-effect of mutating the dataframe's
state).
}
