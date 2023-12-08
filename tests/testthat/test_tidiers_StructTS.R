# FUNCTION: sw_*.StructTS -----
test_that("sw_*.StructTS test returns tibble with correct rows and columns.", {

    # StructTS()  ----
    fit_StructTS <- WWWusage %>%
        stats::StructTS()

    # sw_tidy ----
    test <- sw_tidy(fit_StructTS)
    expect_s3_class(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 3)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_StructTS)
    expect_s3_class(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    # Test normal
    test <- sw_augment(fit_StructTS, rename_index = "date")
    expect_s3_class(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 4)

    # timetk index tests -----

    # Check warning if no timetk index exists
    expect_warning(
        USAccDeaths %>%
            stats::StructTS() %>%
            sw_augment(timetk_idx = TRUE)
    )

    # Check integration with tk_make_future_timeseries()
    monthly_bike_sales <- bike_sales %>%
        dplyr::mutate(month.date = lubridate::as_date(zoo::as.yearmon(order.date))) %>%
        dplyr::group_by(month.date) %>%
        dplyr::summarize(total.daily.sales = sum(price.ext))

    monthly_bike_sales_ts <- tk_ts(monthly_bike_sales, start = 2011, frequency = 12, silent = TRUE)

    fit <- stats::StructTS(monthly_bike_sales_ts)

    test <- fit %>% sw_augment()
    expect_s3_class(test$index, "yearmon")

    test <- fit %>% sw_augment(timetk_idx = TRUE)
    expect_s3_class(test$index, "Date")

})
