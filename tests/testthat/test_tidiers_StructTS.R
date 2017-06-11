library(sweep)
library(forecast)
context("Testing StructTS tidiers")


# FUNCTION: sw_*.StructTS -----
test_that("sw_*.StructTS test returns tibble with correct rows and columns.", {

    # StructTS()  ----
    fit_StructTS <- WWWusage %>%
        StructTS()

    # sw_tidy ----
    test <- sw_tidy(fit_StructTS)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 3)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_StructTS)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    # Test normal
    test <- sw_augment(fit_StructTS, rename_index = "date")
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 4)

})
