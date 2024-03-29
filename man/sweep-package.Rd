% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sweep-package.R
\docType{package}
\name{sweep-package}
\alias{sweep}
\alias{sweep-package}
\title{sweep: Tidy Tools for Forecasting}
\description{
The \code{sweep} package "tidies" up the modeling workflow of the \code{forecast} package.
}
\details{
The model and forecast objects are not covered by
the \code{broom} package. It includes the \code{\link[=sw_tidy]{sw_tidy()}}, \code{\link[=sw_glance]{sw_glance()}},
and \code{\link[=sw_augment]{sw_augment()}} functions that work in a similar capacity as \code{broom} functions.
In addition, it provides \code{\link[=sw_tidy_decomp]{sw_tidy_decomp()}} to tidy decomposition, and
\code{\link[=sw_sweep]{sw_sweep()}} to coerce \code{forecast} objects to "tibbles" for easy visualization with \code{ggplot2}
and manipulation with \code{dplyr}.

To learn more about \code{sweep}, start with the vignettes:
\code{browseVignettes(package = "sweep")}
}
\seealso{
Useful links:
\itemize{
  \item \url{https://business-science.github.io/sweep/}
  \item \url{https://github.com/business-science/sweep}
  \item Report bugs at \url{https://github.com/business-science/sweep/issues}
}

}
\author{
\strong{Maintainer}: Matt Dancho \email{mdancho@business-science.io}

Authors:
\itemize{
  \item Davis Vaughan \email{dvaughan@business-science.io}
}

}
\keyword{internal}
