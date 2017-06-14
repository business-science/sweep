library(sweep)
library(forecast)
library(tidyquant)
library(timekit)
context("Testing decomposed.ts tidiers")


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

    # timekit index ----

    data_ts <- USAccDeaths %>%
        tk_tbl() %>%
        mutate(index = as_date(index)) %>%
        tk_ts(start = 1973, freq = 12, silent = TRUE)

    fit <- decompose(data_ts)

    # timekit_idx sw_tidy_decomp -----
    test <- fit %>% sw_tidy_decomp()
    expect_equal(class(test$index), "yearmon")

    test <- fit %>% sw_tidy_decomp(timekit_idx = T)
    expect_equal(class(test$index), "Date")

})
