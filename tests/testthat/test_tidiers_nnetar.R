library(sweep)
library(forecast)
context("Testing nnetar tidiers")


# FUNCTION: sw_*.nnetar -----
test_that("sw_*.nnetar test returns tibble with correct rows and columns.", {

    # nnetar()  ----
    fit_nnetar <- USAccDeaths %>%
        nnetar()

    # sw_tidy ----
    test <- sw_tidy(fit_nnetar)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 4)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_nnetar)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    # Test normal
    test <- sw_augment(fit_nnetar, rename_index = "date")
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 4)

    # Test passing data
    test <- sw_augment(fit_nnetar, data = USAccDeaths, rename_index = "date")
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 4)

    # Test passing incorrect data
    expect_warning(sw_augment(fit_nnetar,
                              data = timekit::tk_ts(USAccDeaths[1:50], freq = 12, start = 1973),
                              rename_index = "date")
    )

})
