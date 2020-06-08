#' Tidying methods for ARIMA modeling of time series
#'
#' These methods tidy the coefficients of ARIMA models of univariate time
#' series.
#'
#' @param x An object of class "Arima"
#' @param data Used with `sw_augment` only.
#' `NULL` by default which simply returns augmented columns only.
#' User can supply the original data, which returns the data + augmented columns.
#' @param rename_index Used with `sw_augment` only.
#' A string representing the name of the index generated.
#' @param timetk_idx Used with `sw_augment` only.
#' Uses a irregular timetk index if present.
#'
#'
#' @seealso [arima()], [Arima()]
#'
#' @examples
#' library(dplyr)
#' library(forecast)
#' library(sweep)
#'
#' fit_arima <- WWWusage %>%
#'     auto.arima()
#'
#' sw_tidy(fit_arima)
#' sw_glance(fit_arima)
#' sw_augment(fit_arima)
#'
#'
#' @name tidiers_arima
NULL


#' @rdname tidiers_arima
#'
#' @param ... Additional parameters (not used)
#'
#' @return
#' __`sw_tidy()`__ returns one row for each coefficient in the model,
#' with five columns:
#'   * `term`: The term in the nonlinear model being estimated and tested
#'   * `estimate`: The estimated coefficient
#'
#' @export
sw_tidy.Arima <- function(x, ...) {

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


#' @rdname tidiers_arima
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
sw_glance.Arima <- function(x, ...) {

    # Model description
    ret_1 <- tibble::tibble(model.desc = arima_string(x))

    # Summary statistics
    ret_2 <- tibble::tibble(sigma = sqrt(x$sigma2))
    ret_2 <- finish_glance(ret_2, x) %>%
        tibble::as_tibble()

    # forecast accuracy
    ret_3 <- tibble::as_tibble(forecast::accuracy(x))

    ret <- dplyr::bind_cols(ret_1, ret_2, ret_3)

    return(ret)

}


#' @rdname tidiers_arima
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
sw_augment.Arima <- function(x, data = NULL, rename_index = "index", timetk_idx = FALSE, ...) {

    # Check timetk_idx
    if (timetk_idx) {
        if (!has_timetk_idx(x)) {
            warning("Object has no timetk index. Using default index.")
            timetk_idx = FALSE
        }
    }

    # Convert model to tibble
    if ("fitted" %in% names(x)) {
        # forecast::Arima
        ret <- tk_tbl(cbind(.actual = x$x, .fitted = x$fitted, .resid = x$residuals),
                   rename_index = rename_index, silent = TRUE)

    } else {
        # stats::Arima
        warning("No `.actual` or `.fitted` within stats::arima() models. Use forecast::Arima() if more information is needed.")
        ret <- tk_tbl(x$residuals, rename_index = rename_index, silent = TRUE) %>%
                   dplyr::rename(.resid = value)

    }

    # Apply timetk index if selected
    if (timetk_idx) {
        idx <- tk_index(x, timetk_idx = TRUE)
        ret[, rename_index] <- idx
    }

    # Augment columns if necessary
    ret <- sw_augment_columns(ret, data, rename_index, timetk_idx)

    return(ret)

}
