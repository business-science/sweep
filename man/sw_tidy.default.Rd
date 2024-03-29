% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sw_tidy.R
\name{sw_tidy.default}
\alias{sw_tidy.default}
\title{Default tidying method}
\usage{
\method{sw_tidy}{default}(x, ...)
}
\arguments{
\item{x}{an object to be tidied}

\item{...}{extra arguments passed to \code{broom::tidy()}}
}
\value{
A tibble generated by \code{\link[broom:reexports]{broom::tidy()}}
}
\description{
By default, \code{sw_tidy()} uses \code{\link[broom:reexports]{broom::tidy()}} to convert its output.
}
