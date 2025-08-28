#' Tidy forecast objects
#'
#' @param x A time-series forecast of class `forecast`.
#' @param fitted Whether or not to return the fitted values (model values) in the results.
#' FALSE by default.
#' @param rename_index Enables the index column to be renamed.
#' @param timetk_idx If timetk index (non-regularized index) is present, uses it
#' to develop forecast. Otherwise uses default index.
#' @param ... Additional arguments passed to [timetk::tk_make_future_timeseries()]
#'
#' @return Returns a `tibble` object.
#'
#' @details `sw_sweep` is designed
#' to coerce `forecast` objects from the `forecast` package
#' into `tibble` objects in a "tidy" format (long).
#' The returned object contains both the actual values
#' and the forecasted values including the point forecast and upper and lower
#' confidence intervals.
#'
#' The `timetk_idx` argument is used to modify the return format of the index.
#'
#' * If `timetk_idx = FALSE`, a regularized time index is always constructed.
#' This may be in the format of numeric values (e.g. 2010.000) or the
#' higher order `yearmon` and `yearqtr` classes from the `zoo` package.
#' A higher order class is attempted to be returned.
#'
#' * If `timetk_idx = TRUE` and a timetk index is present, an irregular time index
#' will be returned that combines the original time series (i.e. date or datetime)
#' along with a computed future time series created using [timetk::tk_make_future_timeseries()]
#' from the `timetk` package. The `...` can be used to pass additional arguments
#' to [timetk::tk_make_future_timeseries()] such as `inspect_weekdays`, `skip_values`, etc
#' that can be useful in tuning the future time series sequence.
#'
#' The index column name can be changed using the `rename_index` argument.
#'
#' @seealso [timetk::tk_make_future_timeseries()]
#'
#' @examples
#' library(forecast)
#' library(dplyr)
#'
#' # ETS forecasts
#' USAccDeaths %>%
#'     ets() %>%
#'     forecast(level = c(80, 95, 99)) %>%
#'     sw_sweep()
#'
#'
#' @export
sw_sweep <- function(x, fitted = FALSE, timetk_idx = FALSE, rename_index = "index", ...) {
    UseMethod("sw_sweep", x)
}

#' @export
sw_sweep.forecast <- function(x, fitted = FALSE, timetk_idx = FALSE, rename_index = "index", ...) {

    # Check timetk_idx
    if (timetk_idx)
        if (!has_timetk_idx(x)) {
            warning("Object has no timetk index. Using default index.")
            timetk_idx = FALSE
        }

    # Get tibbles from forecast model
    if (timetk_idx) {
        # If timetk index desired
        ret_x     <- tk_tbl(x$x, preserve_index = TRUE, rename_index = rename_index, timetk_idx = timetk_idx, silent = TRUE)
        if (fitted) {
            ret_fit   <- tk_tbl(x$fitted, preserve_index = TRUE, rename_index = rename_index, silent = TRUE)
            ret_fit[, rename_index] <- ret_x[, rename_index]
        }
        # Use tk_make_future_timeseries() to build
        n <- length(x$mean)
        future_idx <- ret_x %>%
            timetk::tk_index() %>%
            timetk::tk_make_future_timeseries(length_out = n, ...)
        future_idx <- future_idx[1:n]
        ret_mean  <- tk_tbl(x$mean, preserve_index = TRUE, rename_index = rename_index, silent = TRUE)
        ret_mean[, rename_index] <- future_idx

    } else {
        ret_x     <- tk_tbl(x$x, preserve_index = TRUE, rename_index = rename_index, silent = TRUE)
        if (fitted) ret_fit   <- tk_tbl(x$fitted, preserve_index = TRUE, rename_index = rename_index, silent = TRUE)
        ret_mean  <- tk_tbl(x$mean, preserve_index = TRUE, rename_index = rename_index, silent = TRUE)
    }

    # Add key column
    ret_x <- ret_x %>%
        tibble::add_column(key = rep("actual", nrow(.)))
    if (fitted) {
        ret_fit <- ret_fit %>%
            tibble::add_column(key = rep("fitted", nrow(.)))
    }
    ret_mean <- ret_mean %>%
        tibble::add_column(key = rep("forecast", nrow(.)))

    ret_fcast <- ret_mean
    if (!is.null(x$level)) {
        # If levels, add columns to forecast
        ret_upper <- tk_tbl(x$upper, preserve_index = FALSE, silent = TRUE)
        ret_lower <- tk_tbl(x$lower, preserve_index = FALSE, silent = TRUE)
        # Fix colnames
        colnames(ret_upper) <- stringr::str_c("hi.", x$level)
        colnames(ret_lower) <- stringr::str_c("lo.", x$level)
        # Combine into forecast
        ret_fcast <- dplyr::bind_cols(ret_mean, ret_lower, ret_upper)
    }

    # Validate indexes
    ret_x_has_index <- rename_index %in% colnames(ret_x)
    if (fitted) {
        ret_fit_has_index <- rename_index %in% colnames(ret_fit)
    } else {
        ret_fit_has_index <- TRUE
    }
    ret_fcast_has_index <- rename_index %in% colnames(ret_fcast)

    # If no index, drop index columns and auto.index
    if (!ret_x_has_index || !ret_fcast_has_index || !ret_fit_has_index) {

        if (ret_x_has_index) ret_x <- dplyr::select(ret_x, -1)
        if (fitted) {
            if (ret_fit_has_index) ret_fit <- dplyr::select(ret_fcast, -1)
        }
        if (ret_fcast_has_index) ret_fcast <- dplyr::select(ret_fcast, -1)

        ret_x_auto_index <- 1:nrow(ret_x)
        if (fitted)ret_fit_auto_index <- 1:nrow(ret_fit)
        ret_fcast_auto_index <- seq(from = nrow(ret_x) + 1, length.out = nrow(ret_fcast))

        ret_x <- ret_x %>%
            tibble::add_column(index = ret_x_auto_index)
        if (fitted) {
            ret_fit <- ret_fit %>%
                tibble::add_column(index = ret_fit_auto_index)
        }
        ret_fcast <- ret_fcast %>%
            tibble::add_column(index = ret_fcast_auto_index)
    }

    # Make column names containing values same
    if (fitted) colnames(ret_fit)[[2]]   = colnames(ret_x)[[2]]
    colnames(ret_fcast)[[2]] = colnames(ret_x)[[2]]

    # Bind Rows
    ret <- ret_x
    if (fitted) {
        ret <- rbind(ret_x, ret_fit) %>%
            dplyr::select(!! rlang::sym(rename_index), key, dplyr::everything())
    }
    if (ncol(ret) != ncol(ret_fcast)) {
        colnames_to_add <- colnames(ret_fcast)[!(colnames(ret_fcast) %in% colnames(ret))]
        for (i in seq_along(colnames_to_add)) ret[,colnames_to_add[i]] <- NA
    }
    ret <- rbind(ret, ret_fcast) %>%
        dplyr::select(!! rlang::sym(rename_index), key, dplyr::everything())

    return(ret)
}

#' @export
sw_sweep.default <- function(x, fitted = TRUE, timetk_idx = FALSE, rename_index = "index", ...) {
    warning(paste0("`sw_sweep` function does not support class ", class(x)[[1]],
                   ". Object must inherit forecast class. Returning `x`."))
    return(x)
}
