
<!-- README.md is generated from README.Rmd. Please edit that file -->
sweep
=====

[![Travis-CI Build Status](https://travis-ci.org/business-science/sweep.svg?branch=master)](https://travis-ci.org/business-science/sweep.svg?branch=master) [![codecov](https://codecov.io/gh/business-science/sweep/branch/master/graph/badge.svg)](https://codecov.io/gh/business-science/sweep) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/sweep)](https://cran.r-project.org/package=sweep) ![](http://cranlogs.r-pkg.org/badges/sweep?color=brightgreen) ![](http://cranlogs.r-pkg.org/badges/grand-total/sweep?color=brightgreen)

> Extending `broom` to time series forecasting

The `sweep` package extends the `broom` tools (tidy, glance, and augment) for performing forecasts and time series analysis in the "tidyverse". The package is geared towards the workflow required to perform forecasts using Rob Hyndman's `forecast` package, and it contains elements that can help when performing time series analysis using tibbles ("tidy" data frames).

Benefits
--------

-   **Designed for modeling and scaling forecasts using the the `tidyverse` tools in [*R for Data Science*](http://r4ds.had.co.nz/)**
-   **Extends `broom` for model analysis (ARIMA, ETS, BATS, etc)**
-   **Tidies the `forecast()` output for easy plotting and "tidy" data manipulation**
-   **Integrates `timekit` to enable working with dates and datetimes (irregular time series) as well as numeric time series (regular time series)**

Tools
-----

The package contains the following elements:

1.  **model tidiers**: `sw_tidy`, `sw_glance`, `sw_augment`, `sw_tidy_decomp` functions extend `tidy`, `glance`, and `augment` from the `broom` package specifically for models (`ets()`, `Arima()`, `bats()`, etc) used for forecasting.

2.  **forecast tidier**: `sw_sweep` converts a `forecast` object to a tibble that can be easily manipulated in the "tidyverse".

Making forecasts in the tidyverse
---------------------------------

`sweep` enables transitioning from tibble to ts, from ts to model (e.g. Arima, ets, etc), from model to forecast, and then from forecast to tibble. The result is ability to use `dplyr`, `tidyr`, and `ggplot` natively to manipulate, analyze and visualize forecasts.

<img src="img/forecast.png" width="100%" />

Forecasting multiple time series groups at scale
------------------------------------------------

Often forecasts are required on grouped data to analyse trends in sub-categories. The good news is scaling from one time series to many is easy with the various `sw_` functions in combination with `dplyr` and `purrr`.

<img src="img/time_series_groups.png" width="100%" />

Forecasting multiple models for accuracy
----------------------------------------

A common goal in forecasting is to compare different forecast models against each other. `sweep` helps in this area as well.

<img src="img/multiple_models.png" width="100%" />

broom extensions for forecasting
--------------------------------

If you are familiar with `broom`, you know how useful it is for retrieving "tidy" format model coefficients (`tidy`), accuracy statistics (`glance`), and residuals (`augment`). The `sweep` package extends these functions to forecast modeling functions such as ARIMA, ETS, BATS, TBATS, NNETAR, and more: just use the `sweep` functions, `sw_tidy`, `sw_glance`, and `sw_augment`. In addition, new tidiers, `sw_tidy_decomp` is designed to specifically tidy models that produce seasonal decompositions and `sw_sweep` is designed to tidy forecast prediction outputs. The compatibility chart is listed below.

| Function      | sw\_tidy() | sw\_glance() | sw\_augment() | sw\_tidy\_decomp() | sw\_sweep() |
|:--------------|:----------:|:------------:|:-------------:|:------------------:|:-----------:|
| ar()          |            |              |               |                    |             |
| arima()       |      X     |       X      |       X       |                    |             |
| Arima()       |      X     |       X      |       X       |                    |             |
| ets()         |      X     |       X      |       X       |          X         |             |
| baggedETS()   |            |              |               |                    |             |
| bats()        |      X     |       X      |       X       |          X         |             |
| tbats()       |      X     |       X      |       X       |          X         |             |
| nnetar()      |      X     |       X      |       X       |                    |             |
| stl()         |            |              |               |          X         |             |
| HoltWinters() |      X     |       X      |       X       |          X         |             |
| StructTS      |      X     |       X      |       X       |          X         |             |
| tslm()        |      X     |       X      |       X       |                    |             |
| decompose()   |            |              |               |          X         |             |
| adf.test()    |      X     |       X      |               |                    |             |
| Box.test()    |      X     |       X      |               |                    |             |
| kpss.test()   |      X     |       X      |               |                    |             |
| forecast()    |            |              |               |                    |      X      |

Installation
------------

Here's how to install to get started.

Development version with latest features:

``` r
# install.packages("devtools")
devtools::install_github("business-science/sweep")
```

<!-- CRAN approved version: -->
<!-- ```{r, eval = FALSE} -->
<!-- install.packages("sweep") -->
<!-- ``` -->
Further Information
-------------------

The `sweep` package includes several vignettes to help users get up to speed quickly:

-   SW00 - Introduction to `sweep`
-   SW01 - Forecasting Time Series Groups in the tidyverse
-   SW02 - Forecasting Using Multiple Models

<!-- See the [`tidyquant` vignettes](https://cran.r-project.org/package=tidyquant) for further details on the package. -->
