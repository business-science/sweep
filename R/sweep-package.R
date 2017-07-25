#' sweep: Extending broom to time series forecasting
#'
#' @details
#' The `sweep` package "tidies" up the
#' modeling workflow of the `forecast` package.
#' The model and forecast objects are not covered by
#' the `broom` package. It includes the [sw_tidy()], [sw_glance()],
#' and [sw_augment()] functions that work in a similar capacity as `broom` functions.
#' In addition, it provides [sw_tidy_decomp()] to tidy decompositions, and
#' [sw_sweep()] to coerce `forecast` objects to "tibbles" for easy visualization with `ggplot2`
#' and manipulation with `dplyr`.
#'
#' To learn more about `sweep`, start with the vignettes:
#'  `browseVignettes(package = "sweep")`
#'
#' @docType package
#' @name sweep_package
#'
#' @importFrom dplyr %>%
#' @importFrom timetk tk_tbl tk_ts tk_index has_timetk_idx tk_get_timeseries_variables

NULL
