#' Coerces decomposed time-series objects to tibble format.
#'
#' @param x A time-series object of class `stl`, `ets`, `decomposed.ts`, `HoltWinters`,
#'  `bats` or `tbats`.
#' @param rename_index Enables the index column to be renamed.
#' @param ... Additional parameters passed to the [tibble::as_tibble()] function.
#'
#' @return Returns a `tibble` object.
#'
#' @details `sw_tidy_decomp` is designed
#' to coerce time-series objects with decompositions to `tibble` objects.
#'
#' A regularized time index is always constructed. If no time index is
#' detected, a sequential index is returned as a default.
#' The index column name can be changed using the `rename_index` argument.
#'
#' @examples
#' library(forecast)
#' library(sweep)
#'
#' # Decompose ETS model
#' USAccDeaths %>%
#'     ets() %>%
#'     sw_tidy_decomp()
#'
#' # Decompose STL object
#' USAccDeaths %>%
#'     stl(s.window = 'periodic') %>%
#'     sw_tidy_decomp()
#'
#'
#' @export
sw_tidy_decomp <- function(x, rename_index = "index", ...) {
    UseMethod("sw_tidy_decomp", x)
}

#' @export
sw_tidy_decomp.ets <- function(x, rename_index = "index", ...) {

    # Get tibbles from ets model
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
    ret <- tk_tbl(ret, preserve_index = TRUE, rename_index, silent = TRUE, ...)

    # Index using sw_augment_columns() with data = NULL
    ret <- sw_augment_columns(ret, data = NULL, rename_index = rename_index)

    return(ret)
}

#' @export
sw_tidy_decomp.stl <- function(x, rename_index = "index", ...) {

    ret <- cbind(seasonal    = x$time.series[,1],
                 trend       = x$time.series[,2],
                 remainder   = x$time.series[,3],
                 seasadj     = forecast::seasadj(x))

    # Coerce to tibble
    ret <- tk_tbl(ret, preserve_index = TRUE, rename_index, silent = TRUE, ...)

    # Index using sw_augment_columns() with data = NULL
    ret <- sw_augment_columns(ret, data = NULL, rename_index = rename_index)

    return(ret)
}

#' @export
sw_tidy.stl <- function(x, ...) {
    message("Using `sw_tidy_decomp()`...")
    sw_tidy_decomp(x, ...)
}

#' @export
sw_tidy_decomp.stlm <- function(x, rename_index = "index", ...) {

    ret <- sw_tidy_decomp(x$stl, rename_index, ...)

    return(ret)
}

#' @export
sw_tidy_decomp.decomposed.ts <- function(x, rename_index = "index", ...) {

    ret <- cbind(actual   = x$x,
                 seasonal = x$seasonal,
                 trend    = x$trend,
                 random   = x$random,
                 seasadj  = forecast::seasadj(x))

    # Coerce to tibble
    ret <- tk_tbl(ret, preserve_index = TRUE, rename_index, silent = TRUE, ...)

    # Index using sw_augment_columns() with data = NULL
    ret <- sw_augment_columns(ret, data = NULL, rename_index = rename_index)

    return(ret)
}

#' @export
sw_tidy_decomp.bats <- function(x, rename_index = "index", ...) {

    ret <- forecast::tbats.components(x)

    # Coerce to tibble
    ret <- tk_tbl(ret, preserve_index = TRUE, rename_index, silent = TRUE, ...)

    # Index using sw_augment_columns() with data = NULL
    ret <- sw_augment_columns(ret, data = NULL, rename_index = rename_index)

    # Add seasadj if season present
    if ("season" %in% colnames(ret)) {
        ret <- ret %>%
            dplyr::mutate(seasadj = observed - season)
    }

    return(ret)
}

#' @export
sw_tidy_decomp.HoltWinters <- function(x, rename_index = "index", ...) {

    ret <- cbind(actual      = x$x,
                 seasonal    = x$fitted[,"season"],
                 trend       = x$fitted[,"trend"])

    # Coerce to tibble
    ret <- tk_tbl(ret, preserve_index = TRUE, rename_index, silent = TRUE, ...)

    ret <- ret %>%
        dplyr::mutate(remainder = actual - seasonal - trend,
                      seasadj   = trend + remainder) %>%
        dplyr::select(-actual)

    # Index using sw_augment_columns() with data = NULL
    ret <- sw_augment_columns(ret, data = NULL, rename_index = rename_index)

    return(ret)
}

#' @export
sw_tidy_decomp.default <- function(x, rename_index = "index", ...) {
    warning(paste0("`sw_tidy_decomp` function does not support class ", class(x)[[1]],
                   "."))
    return(x)
}
