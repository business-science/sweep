library(sweep)
library(forecast)
library(tidyquant)
library(timekit)
context("Testing decomposed.ts functions")


# FUNCTION sw_*.decomposed.ts ----
test_that("sw_*.decomposed.ts test returns tibble with correct rows and columns.", {

    # decompose()  ----
    fit_decomposed_ts <- USAccDeaths %>%
        decompose()

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_decomposed_ts)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 6)

})
