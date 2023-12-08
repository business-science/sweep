# sweep (development version)

* sweep no longer imports tidyverse. (@olivroy, #22)

* Remove internal usage of `dplyr::select_()`. (@olivroy, #22)

# sweep 0.2.5

* Fixes to get `sweep` back on CRAN following inadvertent `timetk` archival. 

# sweep 0.2.4

* Remove support for `robets`.

# sweep 0.2.3

* Fixes for compatability with `broom` v0.7.0
* Add tidiers for `stlm()` models

# sweep 0.2.2

* Fixes for compatability with `tidyquant` v0.5.7

# sweep 0.2.1

* Fixes for `forecast::mstl`

# sweep 0.2.0

* Change to `timetk` from `timekit`.
* Fix Issue #2 - `sw_tidy` fails when `auto.arima()` returns no terms (coefficients).


# sweep 0.1.0 

* Initial release of `sweep`, a tool to "tidy" the forecast modeling and prediction workflow.
