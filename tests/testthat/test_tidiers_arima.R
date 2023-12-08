# FUNCTION: sw_*.Arima -----
test_that("sw_*.Arima test returns tibble with correct rows and columns.", {

    # Arima ----
    fit_arima <- WWWusage %>%
        forecast::auto.arima()

    # sw_tidy ----
    test <- sw_tidy(fit_arima)
    expect_s3_class(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 2)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_arima)
    expect_s3_class(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    test <- sw_augment(fit_arima, rename_index = "date")
    expect_s3_class(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 4)
    expect_equal(colnames(test)[[1]], "date")

    # arima() ----
    fit_arima_stats <- WWWusage %>%
        stats::arima(order = c(1, 1, 1))

    # sw_tidy ----
    test <- sw_tidy(fit_arima_stats)
    expect_s3_class(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 2)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- suppressWarnings(sw_glance(fit_arima_stats))
    expect_s3_class(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 10)
    expect_warning(sw_glance(fit_arima_stats)) # Warning: training accuracy must be within sample

    # sw_augment ----
    test <- suppressWarnings(sw_augment(fit_arima_stats, rename_index = "date"))
    expect_s3_class(test, "tbl")
    # expect_false(any(lapply(test, is.factor) %>% unlist())) # No factors
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 2) # stats::arima() returns only one column for residuals
    expect_equal(colnames(test)[[1]], "date")
    expect_warning(sw_augment(fit_arima_stats)) # stats::arima() vs forecast::Arima()



    # timetk index tests -----

    # Check warning if no timetk index exists
    expect_warning(
        WWWusage %>%
            forecast::auto.arima() %>%
            sw_augment(timetk_idx = TRUE)
    )

    # Test sw_augment
    monthly_bike_sales <- bike_sales %>%
        dplyr::mutate(month.date = lubridate::as_date(zoo::as.yearmon(order.date))) %>%
        dplyr::group_by(month.date) %>%
        dplyr::summarize(total.daily.sales = sum(price.ext))

    monthly_bike_sales_ts <- tk_ts(monthly_bike_sales, start = 2011, freq = 12, silent = TRUE)

    fit <- forecast::auto.arima(monthly_bike_sales_ts)

    test <- fit %>% sw_augment()
    expect_s3_class(test$index, "yearmon")

    test <- fit %>% sw_augment(timetk_idx = TRUE)
    expect_s3_class(test$index, "Date")

    # agument data = ts

    test <- fit %>% sw_augment(data = monthly_bike_sales_ts, timetk_idx = F)
    expect_s3_class(test$index, "yearmon")
    expect_equal(colnames(test)[[2]], "total.daily.sales")

    test <- fit %>% sw_augment(data = monthly_bike_sales_ts, timetk_idx = T)
    expect_s3_class(test$index, "Date")

    test <- fit %>% sw_augment(data = monthly_bike_sales_ts, timetk_idx = TRUE, rename_index = "date")
    expect_s3_class(test$date, "Date")

    # augment data = tbl

    test <- fit %>% sw_augment(data = monthly_bike_sales, timetk_idx = FALSE)
    expect_s3_class(test$month.date, "Date")
    expect_equal(colnames(test)[[2]], "total.daily.sales")

    expect_warning(fit %>% sw_augment(data = monthly_bike_sales, timetk_idx = TRUE))

})
