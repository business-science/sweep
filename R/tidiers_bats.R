#' Tidying methods for BATS and TBATS modeling of time series
#'
#'
#' @param x An object of class "bats" or "tbats"
#' @param data Used with `sw_augment` only.
#' `NULL` by default which simply returns augmented columns only.
#' User can supply the original data, which returns the data + augmented columns.
#' @param rename_index Used with `sw_augment` only.
#' A string representing the name of the index generated.
#' @param timetk_idx Used with `sw_augment` and `sw_tidy_decomp`.
#' When `TRUE`, uses a timetk index (irregular, typically date or datetime) if present.
#' @param ... Additional parameters (not used)
#'
#'
#' @seealso [bats()], [tbats()]
#'
#' @examples
#' library(dplyr)
#' library(forecast)
#' library(sweep)
#'
#' fit_bats <- WWWusage %>%
#'     bats()
#'
#' sw_tidy(fit_bats)
#' sw_glance(fit_bats)
#' sw_augment(fit_bats)
#'
#' @name tidiers_bats
NULL


#' @rdname tidiers_bats
#'
#' @return
#' __`sw_tidy()`__ returns one row for each model parameter,
#' with two columns:
#'   * `term`: The various parameters (lambda, alpha, gamma, etc)
#'   * `estimate`: The estimated parameter value
#'
#'
#' @export
sw_tidy.bats <- function(x, ...) {

    n <- which(stringr::str_detect(names(x), "likelihood")) - 1

    # Collect terms up to but not including likelihood
    x_subset <- x[1:n]

    ret_1 <- tibble::tibble(term = names(x_subset))
    ret_2 <- data.frame(estimate = unlist(x_subset)) %>%
        tibble::rownames_to_column(var = "term")

    ret <- dplyr::left_join(ret_1, ret_2, by = "term")

    return(ret)
}


#' @rdname tidiers_bats
#'
#' @return
#' __`sw_glance()`__ returns one row with the columns
#' * `model.desc`: A description of the model including the
#'   three integer components (p, d, q) are the AR order,
#'   the degree of differencing, and the MA order.
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
sw_glance.bats <- function(x, ...) {

    # Model description
    if (inherits(x, "tbats")) {
        ret_1 <- tibble::tibble(model.desc = tbats_string(x))
    } else {
        ret_1 <- tibble::tibble(model.desc = bats_string(x))
    }

    # Summary statistics
    ret_2 <- tibble::tibble(sigma  = sqrt(x$variance),
                            logLik = x$likelihood,
                            AIC    = x$AIC,
                            BIC    = x$AIC - (2*2) + log(length(x$y))*2)

    # forecast accuracy
    ret_3 <- tibble::as_tibble(forecast::accuracy(x))

    ret <- dplyr::bind_cols(ret_1, ret_2, ret_3)

    return(ret)

}


#' @rdname tidiers_bats
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
sw_augment.bats <- function(x, data = NULL, rename_index = "index", timetk_idx = FALSE, ...) {

    # Check timetk_idx
    if (timetk_idx) {
        if (!has_timetk_idx(x)) {
            warning("Object has no timetk index. Using default index.")
            timetk_idx = FALSE
        }
    }

    # Convert model to tibble
    ret <- tk_tbl(cbind(.actual = x$y, .fitted = x$fitted.values, .resid = x$y - x$fitted.values),
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

#' @rdname tidiers_bats
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
sw_tidy_decomp.bats <- function(x, timetk_idx = FALSE, rename_index = "index", ...) {

    # Check timetk_idx
    if (timetk_idx) {
        if (!has_timetk_idx(x)) {
            warning("Object has no timetk index. Using default index.")
            timetk_idx = FALSE
        }
    }

    # Extract components
    ret <- forecast::tbats.components(x)

    # Coerce to tibble
    ret <- tk_tbl(ret, preserve_index = TRUE, rename_index, silent = TRUE)

    # Add seasadj if season present
    if ("season" %in% colnames(ret)) {
        ret <- ret %>%
            dplyr::mutate(seasadj = observed - season)
    }

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
