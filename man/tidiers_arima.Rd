% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidiers_arima.R, R/tidiers_stl.R
\name{tidiers_arima}
\alias{tidiers_arima}
\alias{sw_tidy.Arima}
\alias{sw_glance.Arima}
\alias{sw_augment.Arima}
\alias{sw_tidy.stlm}
\title{Tidying methods for ARIMA modeling of time series}
\usage{
\method{sw_tidy}{Arima}(x, ...)

\method{sw_glance}{Arima}(x, ...)

\method{sw_augment}{Arima}(x, data = NULL, rename_index = "index", timetk_idx = FALSE, ...)

\method{sw_tidy}{stlm}(x, ...)
}
\arguments{
\item{x}{An object of class "Arima"}

\item{...}{Additional parameters (not used)}

\item{data}{Used with \code{sw_augment} only.
\code{NULL} by default which simply returns augmented columns only.
User can supply the original data, which returns the data + augmented columns.}

\item{rename_index}{Used with \code{sw_augment} only.
A string representing the name of the index generated.}

\item{timetk_idx}{Used with \code{sw_augment} only.
Uses a irregular timetk index if present.}
}
\value{
\strong{\code{sw_tidy()}} returns one row for each coefficient in the model,
with five columns:
\itemize{
\item \code{term}: The term in the nonlinear model being estimated and tested
\item \code{estimate}: The estimated coefficient
}

\strong{\code{sw_glance()}} returns one row with the columns
\itemize{
\item \code{model.desc}: A description of the model including the
three integer components (p, d, q) are the AR order,
the degree of differencing, and the MA order.
\item \code{sigma}: The square root of the estimated residual variance
\item \code{logLik}: The data's log-likelihood under the model
\item \code{AIC}: The Akaike Information Criterion
\item \code{BIC}: The Bayesian Information Criterion
\item \code{ME}: Mean error
\item \code{RMSE}: Root mean squared error
\item \code{MAE}: Mean absolute error
\item \code{MPE}: Mean percentage error
\item \code{MAPE}: Mean absolute percentage error
\item \code{MASE}: Mean absolute scaled error
\item \code{ACF1}: Autocorrelation of errors at lag 1
}

\strong{\code{sw_augment()}} returns a tibble with the following time series attributes:
\itemize{
\item \code{index}: An index is either attempted to be extracted from the model or
a sequential index is created for plotting purposes
\item \code{.actual}: The original time series
\item \code{.fitted}: The fitted values from the model
\item \code{.resid}: The residual values from the model
}

\strong{\code{sw_tidy()}} returns the underlying ETS or ARIMA model's \code{sw_tidy()}
one row for each coefficient in the model,
with five columns:
\itemize{
\item \code{term}: The term in the nonlinear model being estimated and tested
\item \code{estimate}: The estimated coefficient
}
}
\description{
These methods tidy the coefficients of ARIMA models of univariate time
series.
}
\examples{
library(dplyr)
library(forecast)

fit_arima <- WWWusage \%>\%
    auto.arima()

sw_tidy(fit_arima)
sw_glance(fit_arima)
sw_augment(fit_arima)


}
\seealso{
\code{\link[=arima]{arima()}}, \link[=arima]{Arima()}
}
