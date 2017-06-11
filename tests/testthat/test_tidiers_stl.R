library(sweep)
library(forecast)
context("Testing STL tidiers")


# FUNCTION sw_*.stl -----
test_that("sw_*.stl test returns tibble with correct rows and columns.", {

    # stl()  ----
    fit_stl <- USAccDeaths %>%
        stl(s.window = 'periodic')

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_stl)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 5)

    # stlm() ----
    fit_stlm <- USAccDeaths %>%
        stlm(modelfunction=ar)

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_stlm)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 5)
})
