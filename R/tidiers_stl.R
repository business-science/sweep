#' Tidying methods for STL (Seasonal, Trend, Level) decomposition of time series
#'
#'
#' @param x An object of class "stl"
#' @param rename_index Used with `sw_tidy_decomp`.
#' A string representing the name of the index generated.
#' @param timetk_idx Used with `sw_tidy_decomp`.
#' When `TRUE`, uses a timetk index (irregular, typically date or datetime) if present.
#' @param ... Not used.
#'
#'
#' @seealso [stats::stl()]
#'
#' @examples
#' library(dplyr)
#' library(forecast)
#' library(sweep)
#'
#' fit_stl <- USAccDeaths %>%
#'     stl(s.window = "periodic")
#'
#' sw_tidy_decomp(fit_stl)
#'
#' @name tidiers_stl
NULL

#' @rdname tidiers_stl
#'
#' @return
#' __`sw_tidy()`__ wraps `sw_tidy_decomp()`
#'
#'
#' @export
sw_tidy.stl <- function(x, ...) {
    message("Using `sw_tidy_decomp()`...")
    sw_tidy_decomp(x, ...)
}

#' @rdname tidiers_stl
#'
#' @return
#' __`sw_tidy_decomp()`__ returns a tibble with the following time series attributes:
#'   * `index`: An index is either attempted to be extracted from the model or
#'   a sequential index is created for plotting purposes
#'   * `season`: The seasonal component
#'   * `trend`: The trend component
#'   * `remainder`: observed - (season + trend)
#'   * `seasadj`: observed - season (or trend + remainder)
#'
#' @export
sw_tidy_decomp.stl <- function(x, timetk_idx = FALSE, rename_index = "index", ...) {

    # Check timetk_idx
    if (timetk_idx) {
        if (!has_timetk_idx(x)) {
            warning("Object has no timetk index. Using default index.")
            timetk_idx = FALSE
        }
    }

    # Extract from model
    ret <- cbind(observed    = forecast::seasadj(x) + forecast::seasonal(x),
                 season      = forecast::seasonal(x),
                 trend       = forecast::trendcycle(x),
                 remainder   = forecast::remainder(x),
                 seasadj     = forecast::seasadj(x))

    # Coerce to tibble
    ret <- tk_tbl(ret, preserve_index = TRUE, rename_index, silent = TRUE)

    # Apply timetk index if selected
    if (timetk_idx) {
        idx <- tk_index(x, timetk_idx = TRUE)
        if (nrow(ret) != length(idx)) ret <- ret[(nrow(ret) - length(idx) + 1):nrow(ret),]
        ret[, rename_index] <- idx
    }

    # Index using sw_augment_columns() with data = NULL
    ret <- sw_augment_columns(ret, data = NULL, rename_index = rename_index, timetk_idx = timetk_idx)

    return(ret)
}


#' @rdname tidiers_stl
#'
#' @export
sw_tidy_decomp.stlm <- function(x, timetk_idx = FALSE, rename_index = "index", ...) {

    ret <- sw_tidy_decomp.stl(x$stl, timetk_idx = timetk_idx, rename_index = rename_index, ...)

    return(ret)
}

#' @rdname tidiers_arima
#'
#' @param ... Additional parameters (not used)
#'
#' @return
#' __`sw_tidy()`__ returns the underlying ETS or ARIMA model's `sw_tidy()`
#' one row for each coefficient in the model,
#' with five columns:
#'   * `term`: The term in the nonlinear model being estimated and tested
#'   * `estimate`: The estimated coefficient
#'
#' @export
sw_tidy.stlm <- function(x, ...) {

    sw_tidy(x$model)
}

#' @rdname tidiers_stl
#'
#' @return
#' __`sw_glance()`__ returns the underlying ETS or ARIMA model's `sw_glance()` results one row with the columns
#' * `model.desc`: A description of the model including the
#'   three integer components (p, d, q) are the AR order,
#'   the degree of differencing, and the MA order.
#' * `sigma`: The square root of the estimated residual variance
#' * `logLik`: The data's log-likelihood under the model
#' * `AIC`: The Akaike Information Criterion
#' * `BIC`: The Bayesian Information Criterion
#' * `ME`: Mean error
#' * `RMSE`: Root mean squared error
#' * `MAE`: Mean absolute error
#' * `MPE`: Mean percentage error
#' * `MAPE`: Mean absolute percentage error
#' * `MASE`: Mean absolute scaled error
#' * `ACF1`: Autocorrelation of errors at lag 1
#'
#' @export
sw_glance.stlm <- function(x, ...) {
    sw_glance(x$model)
}

#' @rdname tidiers_stl
#'
#' @param data Used with `sw_augment` only.
#'
#' @return
#' __`sw_augment()`__ returns a tibble with the following time series attributes:
#'   * `index`: An index is either attempted to be extracted from the model or
#'   a sequential index is created for plotting purposes
#'   * `.actual`: The original time series
#'   * `.fitted`: The fitted values from the model
#'   * `.resid`: The residual values from the model
#'
#' @export
sw_augment.stlm <- function(x, data = NULL, rename_index = "index", timetk_idx = FALSE, ...) {

    # Check timetk_idx
    if (timetk_idx) {
        if (!has_timetk_idx(x$model)) {
            warning("Object has no timetk index. Using default index.")
            timetk_idx = FALSE
        }
    }

    # Convert model to tibble
    ret <- tk_tbl(cbind(.actual = x$x, .fitted = x$fitted, .resid = x$residuals),
                  rename_index = rename_index, silent = TRUE)

    # Apply timetk index if selected
    if (timetk_idx) {
        idx <- tk_index(x$model, timetk_idx = TRUE)
        ret[, rename_index] <- idx
    }

    # Augment columns if necessary
    ret <- sw_augment_columns(ret, data, rename_index, timetk_idx)

    return(ret)

}
