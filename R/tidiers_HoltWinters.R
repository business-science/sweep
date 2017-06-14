#' Tidying methods for HoltWinters modeling of time series
#'
#' These methods tidy `HoltWinters` models of univariate time
#' series.
#'
#' @param x An object of class "HoltWinters"
#' @param data Used with `sw_augment` only.
#' `NULL` by default which simply returns augmented columns only.
#' User can supply the original data, which returns the data + augmented columns.
#' @param rename_index Used with `sw_augment` only.
#' A string representing the name of the index generated.
#' @param timekit_idx Used with `sw_augment` and `sw_tidy_decomp`.
#' When `TRUE`, uses a timekit index (irregular, typically date or datetime) if present.
#' @param ... Additional parameters (not used)
#'
#'
#' @seealso [HoltWinters()]
#'
#' @examples
#' library(forecast)
#' library(sweep)
#'
#' fit_hw <- USAccDeaths %>%
#'     stats::HoltWinters()
#'
#' sw_tidy(fit_hw)
#' sw_glance(fit_hw)
#' sw_augment(fit_hw)
#' sw_tidy_decomp(fit_hw)
#'
#' @name tidiers_HoltWinters
NULL


#' @rdname tidiers_HoltWinters
#'
#'
#' @return
#' __`sw_tidy()`__ returns one row for each model parameter,
#' with two columns:
#'   * `term`: The various parameters (alpha, beta, gamma, and coefficients)
#'   * `estimate`: The estimated parameter value
#'
#'
#' @export
sw_tidy.HoltWinters <- function(x, ...) {

    terms     <- c("alpha", "beta", "gamma", names(stats::coef(x)))
    estimates <- c(x$alpha, x$beta, x$gamma, stats::coef(x))

    ret <- tibble::tibble(term     = terms,
                          estimate = estimates)

    return(ret)
}


#' @rdname tidiers_HoltWinters
#'
#' @return
#' __`sw_glance()`__ returns one row with the following columns:
#' * `model.desc`: A description of the model
#' * `sigma`: The square root of the estimated residual variance
#' * `logLik`: The data's log-likelihood under the model
#' * `AIC`: The Akaike Information Criterion
#' * `BIC`: The Bayesian Information Criterion (`NA` for bats / tbats)
#' * `ME`: Mean error
#' * `RMSE`: Root mean squared error
#' * `MAE`: Mean absolute error
#' * `MPE`: Mean percentage error
#' * `MAPE`: Mean absolute percentage error
#' * `MASE`: Mean absolute scaled error
#' * `ACF1`: Autocorrelation of errors at lag 1
#'
#' @export
sw_glance.HoltWinters <- function(x, ...) {

    # Model description
    ret_1 <- tibble::tibble(model.desc = "HoltWinters")

    # Summary statistics
    ret_2 <- tibble::tibble(sigma  = sqrt(x$SSE),
                            logLik = NA,
                            AIC    = NA,
                            BIC    = NA)

    # forecast accuracy
    ret_3 <- tibble::as_tibble(forecast::accuracy(
        forecast::forecast(x)
    ))

    ret <- dplyr::bind_cols(ret_1, ret_2, ret_3)

    return(ret)

}


#' @rdname tidiers_HoltWinters
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
sw_augment.HoltWinters <- function(x, data = NULL, rename_index = "index", timekit_idx = FALSE, ...) {

    # Check timekit_idx
    if (timekit_idx) {
        if (!has_timekit_idx(x)) {
            warning("Object has no timekit index. Using default index.")
            timekit_idx = FALSE
        }
    }

    # Convert model to tibble
    ret <- tk_tbl(cbind(.actual = x$x, .fitted = x$fitted[,1]),
                  rename_index = rename_index, silent = TRUE)
    ret <- ret %>%
        dplyr::mutate(.resid = .actual - .fitted)

    # Apply timekit index if selected
    if (timekit_idx) {
        idx <- tk_index(x, timekit_idx = TRUE)
        ret[, rename_index] <- idx
    }

    # Augment columns if necessary
    ret <- sw_augment_columns(ret, data, rename_index, timekit_idx)

    return(ret)

}

#' @rdname tidiers_HoltWinters
#'
#' @return
#' __`sw_tidy_decomp()`__ returns a tibble with the following time series attributes:
#'   * `index`: An index is either attempted to be extracted from the model or
#'   a sequential index is created for plotting purposes
#'   * `observed`: The original time series
#'   * `season`: The seasonal component
#'   * `trend`: The trend component
#'   * `remainder`: observed - (season + trend)
#'   * `seasadj`: observed - season (or trend + remainder)
#'
#' @export
sw_tidy_decomp.HoltWinters <- function(x, timekit_idx = FALSE, rename_index = "index", ...) {

    # Check timekit_idx
    if (timekit_idx) {
        if (!has_timekit_idx(x)) {
            warning("Object has no timekit index. Using default index.")
            timekit_idx = FALSE
        }
    }

    # Get tibble from HoltWinters model
    ret <- cbind(observed    = x$x,
                 season      = x$fitted[,"season"],
                 trend       = x$fitted[,"trend"])

    # Coerce to tibble
    ret <- tk_tbl(ret, preserve_index = TRUE, rename_index, silent = TRUE)

    ret <- ret %>%
        dplyr::mutate(remainder = observed - season - trend,
                      seasadj   = trend + remainder)

    # Apply timekit index if selected
    if (timekit_idx) {
        idx <- tk_index(x, timekit_idx = TRUE)
        if (nrow(ret) != length(idx)) ret <- ret[(nrow(ret) - length(idx) + 1):nrow(ret),]
        ret[, rename_index] <- idx
    }

    # Index using sw_augment_columns() with data = NULL
    ret <- sw_augment_columns(ret, data = NULL, rename_index = rename_index, timekit_idx = timekit_idx)

    return(ret)
}
