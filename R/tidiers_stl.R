#' Tidying methods for STL (Seasonal, Trend, Level) decomposition of time series
#'
#'
#' @param x An object of class "stl" or "stlm"
#' @param rename_index Used with `sw_tidy_decomp`.
#' A string representing the name of the index generated.
#' @param timekit_idx Used with `sw_tidy_decomp`.
#' When `TRUE`, uses a timekit index (irregular, typically date or datetime) if present.
#' @param ... Not used.
#'
#'
#' @seealso [stl()]
#'
#' @examples
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
sw_tidy_decomp.stl <- function(x, timekit_idx = FALSE, rename_index = "index", ...) {

    # Check timekit_idx
    if (timekit_idx) {
        if (!has_timekit_idx(x)) {
            warning("Object has no timekit index. Using default index.")
            timekit_idx = FALSE
        }
    }

    # Extract from model
    ret <- cbind(observed    = forecast::seasadj(x) + x$time.series[,1],
                 season      = x$time.series[,1],
                 trend       = x$time.series[,2],
                 remainder   = x$time.series[,3],
                 seasadj     = forecast::seasadj(x))

    # Coerce to tibble
    ret <- tk_tbl(ret, preserve_index = TRUE, rename_index, silent = TRUE)

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


#' @rdname tidiers_stl
#'
#' @export
sw_tidy_decomp.stlm <- function(x, timekit_idx = FALSE, rename_index = "index", ...) {

    ret <- sw_tidy_decomp(x$stl, timekit_idx = timekit_idx, rename_index = rename_index, ...)

    return(ret)
}
