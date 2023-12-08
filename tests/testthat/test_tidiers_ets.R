# FUNCTION: sw_*.ets -----
test_that("sw_*.ets test returns tibble with correct rows and columns.", {

    # ets() ----
    fit_ets <- WWWusage %>%
        forecast::ets()

    # sw_tidy ----
    test <- sw_tidy(fit_ets)
    expect_s3_class(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 5)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_ets)
    expect_s3_class(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    test <- sw_augment(fit_ets, rename_index = "date")
    expect_s3_class(test, "tbl")
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 4)
    expect_equal(colnames(test)[[1]], "date")

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_ets)
    expect_s3_class(test, "tbl")
    expect_equal(nrow(test), 101)
    expect_equal(ncol(test), 4)

    # timetk index tests -----

    # Check warning if no timetk index exists
    expect_warning(
        USAccDeaths %>%
            forecast::ets() %>%
            sw_augment(timetk_idx = TRUE)
    )

    # Check integration with tk_make_future_timeseries()
    monthly_bike_sales <- bike_sales %>%
        dplyr::mutate(month.date = lubridate::as_date(zoo::as.yearmon(order.date))) %>%
        dplyr::group_by(month.date) %>%
        dplyr::summarize(total.daily.sales = sum(price.ext))

    monthly_bike_sales_ts <- tk_ts(monthly_bike_sales, start = 2011, frequency = 12, silent = TRUE)

    fit <- forecast::ets(monthly_bike_sales_ts)

    # sw_augment ----
    test <- fit %>% sw_augment()
    expect_s3_class(test$index, "yearmon")

    test <- fit %>% sw_augment(timetk_idx = T)
    expect_s3_class(test$index, "Date")

    # sw_tidy_decomp -----
    test <- fit %>% sw_tidy_decomp()
    expect_s3_class(test$index, "yearmon")

    test <- fit %>% sw_tidy_decomp(timetk_idx = T)
    expect_s3_class(test$index, "Date")
})
