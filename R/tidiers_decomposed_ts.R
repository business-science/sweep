#' Tidying methods for decomposed time series
#'
#'
#' @param x An object of class "decomposed.ts"
#' @param rename_index Used with `sw_augment` and `sw_tidy_decomp`.
#' A string representing the name of the index generated.
#' @param timetk_idx Used with `sw_augment` and `sw_tidy_decomp`.
#' When `TRUE`, uses a timetk index (irregular, typically date or datetime) if present.
#' @param ... Not used.
#'
#'
#' @seealso [decompose()]
#'
#' @examples
#' library(dplyr)
#' library(forecast)
#'
#' fit_decomposed <- USAccDeaths %>%
#'     decompose()
#'
#' sw_tidy_decomp(fit_decomposed)
#'
#' @name tidiers_decomposed_ts
NULL



#' @rdname tidiers_decomposed_ts
#'
#' @return
#' __`sw_tidy_decomp()`__ returns a tibble with the following time series attributes:
#'   * `index`: An index is either attempted to be extracted from the model or
#'   a sequential index is created for plotting purposes
#'   * `season`: The seasonal component
#'   * `trend`: The trend component
#'   * `random`: The error component
#'   * `seasadj`: observed - season
#'
#' @export
sw_tidy_decomp.decomposed.ts <- function(x, timetk_idx = FALSE, rename_index = "index", ...) {

    # Check timetk_idx
    if (timetk_idx) {
        if (!has_timetk_idx(x$x)) {
            warning("Object has no timetk index. Using default index.")
            timetk_idx = FALSE
        }
    }

    ret <- cbind(observed  = x$x,
                 season    = x$seasonal,
                 trend     = x$trend,
                 random    = x$random,
                 seasadj   = forecast::seasadj(x))

    # Coerce to tibble
    ret <- tk_tbl(ret, preserve_index = TRUE, rename_index, silent = TRUE)

    # Apply timetk index if selected
    if (timetk_idx) {
        idx <- tk_index(x$x, timetk_idx = TRUE)
        if (nrow(ret) != length(idx)) ret <- ret[(nrow(ret) - length(idx) + 1):nrow(ret),]
        ret[, rename_index] <- idx
    }

    # Index using sw_augment_columns() with data = NULL
    ret <- sw_augment_columns(ret, data = NULL, rename_index = rename_index, timetk_idx = timetk_idx)

    return(ret)
}
