test_that("sw_sweep test returns tibble with correct rows and columns.", {

    # ETS forecasts ----
    test_sweep_1 <- USAccDeaths %>%
        forecast::ets() %>%
        forecast::forecast(level = c(80, 95, 99)) %>%
        sw_sweep(fitted = FALSE)

    expect_s3_class(test_sweep_1, "tbl")
    expect_equal(nrow(test_sweep_1), 96)
    expect_equal(ncol(test_sweep_1), 9)
    expect_equal(colnames(test_sweep_1)[[9]], "hi.99")

    test_sweep_1a <- USAccDeaths %>%
        forecast::ets() %>%
        forecast::forecast(level = c(80, 95, 99)) %>%
        sw_sweep(fitted = TRUE)

    expect_equal(nrow(test_sweep_1a), 168)

    # Automatic ARIMA forecasts -----
    expect_equal(
        WWWusage %>%
            forecast::auto.arima() %>%
            forecast::forecast(h=20) %>%
            sw_sweep(fitted = FALSE) %>%
            nrow(),
        120
    )
    expect_equal(
        WWWusage %>%
            forecast::auto.arima() %>%
            forecast::forecast(h=20) %>%
            sw_sweep(fitted = TRUE) %>%
            nrow(),
        220
    )

    # ARFIMA forecasts -----
    x <- fracdiff::fracdiff.sim(100, ma=-.4, d=.3)$series
    expect_equal(
        # Warning: no index
        forecast::arfima(x) %>%
            forecast::forecast(h=30) %>%
            sw_sweep(fitted = FALSE) %>%
            nrow(),
        130
    )
    expect_equal(
        # Warning: no index
        forecast::arfima(x) %>%
            forecast::forecast(h=30) %>%
            sw_sweep(fitted = TRUE) %>%
            nrow(),
        230
    )

    # STL forecasts -----
    test_sweep_2 <- USAccDeaths %>%
        forecast::stlm(modelfunction=ar) %>%
        forecast::forecast(h = 36) %>%
        sw_sweep(fitted = FALSE)

    expect_s3_class(test_sweep_2, "tbl")
    expect_equal(nrow(test_sweep_2), 108)
    expect_equal(ncol(test_sweep_2), 7)
    expect_equal(colnames(test_sweep_2)[[7]], "hi.95")

    test_sweep_2a <- USAccDeaths %>%
        forecast::stlm(modelfunction=ar) %>%
        forecast::forecast(h=36) %>%
        sw_sweep(fitted = T)
    expect_equal(nrow(test_sweep_2a), 180)

    # STLF -----
    test_sweep_3 <- AirPassengers %>%
        forecast::stlf(lambda=0) %>%
        sw_sweep(fitted = F)

    expect_s3_class(test_sweep_3, "tbl")
    expect_equal(nrow(test_sweep_3), 168)
    expect_equal(ncol(test_sweep_3), 7)
    expect_equal(colnames(test_sweep_3)[[7]], "hi.95")

    test_sweep_3a <- AirPassengers %>%
        forecast::stlf(lambda=0) %>%
        sw_sweep(fitted = TRUE)
    expect_equal(nrow(test_sweep_3a), 312)


    # STL -----
    test_sweep_4 <- USAccDeaths %>%
        stats::stl(s.window='periodic') %>%
        forecast::forecast() %>%
        sw_sweep(fitted = FALSE)

    expect_s3_class(test_sweep_4, "tbl")
    expect_equal(nrow(test_sweep_4), 96)
    expect_equal(ncol(test_sweep_4), 7)
    expect_equal(colnames(test_sweep_4)[[7]], "hi.95")

    test_sweep_4a <- USAccDeaths %>%
        stats::stl(s.window='periodic') %>%
        forecast::forecast() %>%
        sw_sweep(fitted = TRUE)
    expect_equal(nrow(test_sweep_4a), 168)

    # TBATS forecast -----
    test_sweep_5 <- USAccDeaths %>%
        forecast::tbats() %>%
        forecast::forecast(level = c(80, 95)) %>%
        sw_sweep(fitted = F)

    expect_s3_class(test_sweep_5, "tbl")
    expect_equal(nrow(test_sweep_5), 96)
    expect_equal(ncol(test_sweep_5), 7)
    expect_equal(colnames(test_sweep_5)[[7]], "hi.95")

    test_sweep_5a <- USAccDeaths %>%
        forecast::tbats() %>%
        forecast::forecast(level = c(80, 95)) %>%
        sw_sweep(fitted = TRUE)
    expect_equal(nrow(test_sweep_5a), 168)

    # sweep.default() -----
    expect_warning(sw_sweep(datasets::mtcars)) # Returns original data and warning message

    # timetk index tests -----

    # Check warning if no timetk index exists
    expect_warning(
        WWWusage %>%
            forecast::auto.arima() %>%
            forecast::forecast() %>%
            sw_sweep(timetk_idx = TRUE)
    )

    # Check integration with tk_make_future_timeseries()
    monthly_bike_sales <- bike_sales %>%
        dplyr::mutate(month.date = lubridate::as_date(zoo::as.yearmon(order.date))) %>%
        dplyr::group_by(month.date) %>%
        dplyr::summarize(total.daily.sales = sum(price.ext))

    monthly_bike_sales_ts <- tk_ts(monthly_bike_sales, start = 2011, freq = 12, silent = TRUE)

    fit <- forecast::auto.arima(monthly_bike_sales_ts)

    fcast <- forecast::forecast(fit)

    test <- sw_sweep(fcast)
    expect_s3_class(test$index, "yearmon", exact = TRUE)

    test <- sw_sweep(fcast, timetk_idx = TRUE)
    expect_s3_class(test$index, "Date", exact = TRUE)

    test <- sw_sweep(fcast, timetk_idx = T, skip_values = lubridate::ymd("2017-12-01")) %>% tail()
    expect_equal(test$index[[6]], lubridate::ymd("2018-01-01"))

    test <- sw_sweep(fcast, fitted = TRUE, timetk_idx = T, skip_values = lubridate::ymd("2017-12-01"))
    expect_equal(test$index[[nrow(test)]], lubridate::ymd("2018-01-01"))
})










