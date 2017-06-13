library(sweep)
library(forecast)
context("Testing bats and tbats tidiers")


# FUNCTION: sw_*.bats -----
test_that("sw_*.bats test returns tibble with correct rows and columns.", {

    # bats()  ----
    fit_bats <- WWWusage %>%
        forecast::bats()

    # sw_tidy ----
    test <- sw_tidy(fit_bats)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 7)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_bats)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    test <- sw_augment(fit_bats, rename_index = "date")
    expect_is(test, "tbl")
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 4)
    expect_equal(colnames(test)[[1]], "date")

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_bats)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 3)


    # tbats()  ----
    fit_tbats <- USAccDeaths %>%
        forecast::tbats()

    # sw_tidy ----
    test <- sw_tidy(fit_tbats)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 8)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_tbats)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    test <- sw_augment(fit_tbats, rename_index = "date")
    expect_is(test, "tbl")
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 4)
    expect_equal(colnames(test)[[1]], "date")

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_tbats)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 5)

    # timekit index tests -----

    # Check warning if no timekit index exists
    expect_warning(
        WWWusage %>%
            bats() %>%
            sw_augment(timekit_idx = T)
    )

    # Check integration with tk_make_future_timeseries()
    monthly_bike_sales <- bike_sales %>%
        mutate(month.date = as_date(as.yearmon(order.date))) %>%
        group_by(month.date) %>%
        summarize(total.daily.sales = sum(price.ext))

    monthly_bike_sales_ts <- tk_ts(monthly_bike_sales, start = 2011, freq = 12, silent = TRUE)

    fit <- bats(monthly_bike_sales_ts)

    test <- fit %>% sw_augment()
    expect_equal(class(test$index), "yearmon")

    test <- fit %>% sw_augment(timekit_idx = T)
    expect_equal(class(test$index), "Date")

})
