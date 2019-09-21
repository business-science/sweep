#' Coerces decomposed time-series objects to tibble format.
#'
#' @param x A time-series object of class `stl`, `ets`, `decomposed.ts`, `HoltWinters`,
#'  `bats` or `tbats`.
#' @param timetk_idx
#' When `TRUE`, uses a timetk index (irregular, typically date or datetime) if present.
#' @param rename_index Enables the index column to be renamed.
#' @param ... Not used.
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
#' library(dplyr)
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
sw_tidy_decomp <- function(x, timetk_idx = FALSE, rename_index = "index", ...) {
    UseMethod("sw_tidy_decomp", x)
}



#' @export
sw_tidy_decomp.default <- function(x, timetk_idx = FALSE, rename_index = "index", ...) {
    warning(paste0("`sw_tidy_decomp` function does not support class ", class(x)[[1]], ". Returning `x`."))
    return(x)
}
