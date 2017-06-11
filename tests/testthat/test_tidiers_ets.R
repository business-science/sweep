library(sweep)
library(forecast)
context("Testing ets tidiers")


# FUNCTION: sw_*.ets -----
test_that("sw_*.ets test returns tibble with correct rows and columns.", {

    # ets() ----
    fit_ets <- WWWusage %>%
        forecast::ets()

    # sw_tidy ----
    test <- sw_tidy(fit_ets)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 5)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_ets)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    test <- sw_augment(fit_ets, rename_index = "date")
    expect_is(test, "tbl")
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 4)
    expect_equal(colnames(test)[[1]], "date")

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_ets)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 101)
    expect_equal(ncol(test), 4)

})
