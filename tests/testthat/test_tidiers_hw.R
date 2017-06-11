library(sweep)
library(forecast)
context("Testing HoltWinters tidiers")

# FUNCTION: sw_*.HoltWinters -----
test_that("sw_*.HoltWinters test returns tibble with correct rows and columns.", {

    # HoltWinters()  ----
    fit_hw <- USAccDeaths %>%
        HoltWinters()

    # sw_tidy ----
    test <- sw_tidy(fit_hw)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 17)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_hw)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    # Test normal
    test <- sw_augment(fit_hw, rename_index = "date")
    expect_is(test, "tbl")
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 4)

    # Test passing data
    test <- sw_augment(fit_hw, data = USAccDeaths, rename_index = "date")
    expect_is(test, "tbl")
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 4)

    # Test passing incorrect data
    expect_warning(sw_augment(fit_hw,
                              data = timekit::tk_ts(USAccDeaths[1:50], freq = 12, start = 1973),
                              rename_index = "date")
    )

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_hw)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 5)

})
