# sweep: Tidy Tools for Forecasting

The `sweep` package "tidies" up the modeling workflow of the `forecast`
package.

## Details

The model and forecast objects are not covered by the `broom` package.
It includes the
[`sw_tidy()`](https://business-science.github.io/sweep/reference/sw_tidy.md),
[`sw_glance()`](https://business-science.github.io/sweep/reference/sw_glance.md),
and
[`sw_augment()`](https://business-science.github.io/sweep/reference/sw_augment.md)
functions that work in a similar capacity as `broom` functions. In
addition, it provides
[`sw_tidy_decomp()`](https://business-science.github.io/sweep/reference/sw_tidy_decomp.md)
to tidy decomposition, and
[`sw_sweep()`](https://business-science.github.io/sweep/reference/sw_sweep.md)
to coerce `forecast` objects to "tibbles" for easy visualization with
`ggplot2` and manipulation with `dplyr`.

To learn more about `sweep`, start with the vignettes:
`browseVignettes(package = "sweep")`

## See also

Useful links:

- <https://business-science.github.io/sweep/>

- <https://github.com/business-science/sweep>

- Report bugs at <https://github.com/business-science/sweep/issues>

## Author

**Maintainer**: Matt Dancho <mdancho@business-science.io>

Authors:

- Davis Vaughan <dvaughan@business-science.io>
