#' Tidying methods for decomposed time series
#'
#'
#' @param x An object of class "decomposed.ts"
#' @param rename_index Used with `sw_augment` and `sw_tidy_decomp`.
#' A string representing the name of the index generated.
#' @param timekit_idx Used with `sw_augment` and `sw_tidy_decomp`.
#' When `TRUE`, uses a timekit index (irregular, typically date or datetime) if present.
#' @param ... Not used.
#'
#'
#' @seealso [decompose()]
#'
#' @examples
#' library(forecast)
#' library(sweep)
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
sw_tidy_decomp.decomposed.ts <- function(x, timekit_idx = FALSE, rename_index = "index", ...) {

    # Check timekit_idx
    if (timekit_idx) {
        if (!has_timekit_idx(x$x)) {
            warning("Object has no timekit index. Using default index.")
            timekit_idx = FALSE
        }
    }

    ret <- cbind(observed  = x$x,
                 season    = x$seasonal,
                 trend     = x$trend,
                 random    = x$random,
                 seasadj   = forecast::seasadj(x))

    # Coerce to tibble
    ret <- tk_tbl(ret, preserve_index = TRUE, rename_index, silent = TRUE)

    # Apply timekit index if selected
    if (timekit_idx) {
        idx <- tk_index(x$x, timekit_idx = TRUE)
        if (nrow(ret) != length(idx)) ret <- ret[(nrow(ret) - length(idx) + 1):nrow(ret),]
        ret[, rename_index] <- idx
    }

    # Index using sw_augment_columns() with data = NULL
    ret <- sw_augment_columns(ret, data = NULL, rename_index = rename_index, timekit_idx = timekit_idx)

    return(ret)
}
