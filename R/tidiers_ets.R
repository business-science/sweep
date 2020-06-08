#' Tidying methods for ETS (Error, Trend, Seasonal) exponential smoothing
#' modeling of time series
#'
#'
#' @param x An object of class "ets"
#' @param data Used with `sw_augment` only.
#' `NULL` by default which simply returns augmented columns only.
#' User can supply the original data, which returns the data + augmented columns.
#' @param rename_index Used with `sw_augment` and `sw_tidy_decomp`.
#' A string representing the name of the index generated.
#' @param timetk_idx Used with `sw_augment` and `sw_tidy_decomp`.
#' When `TRUE`, uses a timetk index (irregular, typically date or datetime) if present.
#' @param ... Not used.
#'
#'
#' @seealso [ets()]
#'
#' @examples
#' library(dplyr)
#' library(forecast)
#' library(sweep)
#'
#' fit_ets <- WWWusage %>%
#'     ets()
#'
#' sw_tidy(fit_ets)
#' sw_glance(fit_ets)
#' sw_augment(fit_ets)
#' sw_tidy_decomp(fit_ets)
#'
#' @name tidiers_ets
NULL


#' @rdname tidiers_ets
#'
#'
#' @return
#' __`sw_tidy()`__ returns one row for each model parameter,
#' with two columns:
#'   * `term`: The smoothing parameters (alpha, gamma) and the initial states
#'   (l, s0 through s10)
#'   * `estimate`: The estimated parameter value
#'
#'
#' @export
sw_tidy.ets <- function(x, ...) {

    coefs <- stats::coef(x)

    if (length(coefs > 0)) {
        ret <- tibble::tibble(
            term      = names(coefs),
            estimate  = coefs
        )
    } else {
        ret <- tibble::tibble(
            term      = NA,
            estimate  = NA
        )
    }

    return(ret)
}


#' @rdname tidiers_ets
#'
#' @return
#' __`sw_glance()`__ returns one row with the columns
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
sw_glance.ets <- function(x, ...) {

    # Model description
    ret_1 <- tibble::tibble(model.desc = x$method)

    # Summary statistics
    ret_2 <- tibble::tibble(sigma = sqrt(x$sigma2))
    ret_2 <- finish_glance(ret_2, x) %>%
        tibble::as_tibble()

    # forecast accuracy
    ret_3 <- tibble::as_tibble(forecast::accuracy(x))

    ret <- dplyr::bind_cols(ret_1, ret_2, ret_3)

    return(ret)

}


#' @rdname tidiers_ets
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
sw_augment.ets <- function(x, data = NULL, timetk_idx = FALSE, rename_index = "index", ...) {

    # Check timetk_idx
    if (timetk_idx) {
        if (!has_timetk_idx(x)) {
            warning("Object has no timetk index. Using default index.")
            timetk_idx = FALSE
        }
    }

    # Convert model to tibble
    ret <- tk_tbl(cbind(.actual = x$x, .fitted = x$fitted, .resid = x$residuals),
                  rename_index = rename_index, silent = TRUE)

    # Apply timetk index if selected
    if (timetk_idx) {
        idx <- tk_index(x, timetk_idx = TRUE)
        ret[, rename_index] <- idx
    }

    # Augment columns if necessary
    ret <- sw_augment_columns(ret, data, rename_index, timetk_idx)

    return(ret)


}

#' @rdname tidiers_ets
#'
#' @return
#' __`sw_tidy_decomp()`__ returns a tibble with the following time series attributes:
#'   * `index`: An index is either attempted to be extracted from the model or
#'   a sequential index is created for plotting purposes
#'   * `observed`: The original time series
#'   * `level`: The level component
#'   * `slope`: The slope component (Not always present)
#'   * `season`: The seasonal component (Not always present)
#'
#' @export
sw_tidy_decomp.ets <- function(x, timetk_idx = FALSE, rename_index = "index", ...) {

    # Check timetk_idx
    if (timetk_idx) {
        if (!has_timetk_idx(x)) {
            warning("Object has no timetk index. Using default index.")
            timetk_idx = FALSE
        }
    }

    # Get tibble from ets model
    # ref: plot.ets
    # https://github.com/robjhyndman/forecast/blob/master/R/ets.R
    if(!is.null(x$lambda))
        y <- forecast::BoxCox(x$x, x$lambda)
    else
        y <- x$x
    if(x$components[3]=="N" & x$components[2]=="N")
    {
        ret <- cbind(observed=y, level=x$states[,1])
    }
    else if(x$components[3]=="N")
    {
        ret <- cbind(observed=y, level=x$states[,1], slope=x$states[,"b"])
    }
    else if(x$components[2]=="N")
    {
        ret <- cbind(observed=y, level=x$states[,1], season=x$states[,"s1"])
    }
    else
    {
        ret <- cbind(observed=y, level=x$states[,1], slope=x$states[,"b"],
                     season=x$states[,"s1"])
    }

    # Coerce to tibble
    ret <- tk_tbl(ret, preserve_index = TRUE, rename_index = rename_index, silent = TRUE)

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
