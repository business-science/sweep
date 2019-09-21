context("Testing lm tidiers")


# FUNCTION: sw_*.default -----
test_that("sw_*.default test returns tibble with correct rows and columns.", {

    # lm() ----
    fit_lm <- lm(mtcars$mpg ~ mtcars$wt)

    # sw_tidy ----
    test <- sw_tidy(fit_lm)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 2)
    expect_equal(ncol(test), 5)

    # sw_glance ----
    test <- sw_glance(fit_lm)
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 11)

    # sw_augment ----
    test <-
        suppressWarnings(
            sw_augment(fit_lm)
        )
    expect_is(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 32)
    expect_equal(ncol(test), 9)

})
