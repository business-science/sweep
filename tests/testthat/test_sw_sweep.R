context("Testing sw_sweep function")


test_that("sw_sweep test returns tibble with correct rows and columns.", {

    # ETS forecasts ----
    test_sweep_1 <- USAccDeaths %>%
        ets %>%
        forecast(level = c(80, 95, 99)) %>%
        sw_sweep(fitted = F)

    expect_is(test_sweep_1, "tbl")
    expect_equal(nrow(test_sweep_1), 96)
    expect_equal(ncol(test_sweep_1), 9)
    expect_equal(colnames(test_sweep_1)[[9]], "hi.99")

    test_sweep_1a <- USAccDeaths %>%
        ets %>%
        forecast(level = c(80, 95, 99)) %>%
        sw_sweep(fitted = T)

    expect_equal(nrow(test_sweep_1a), 168)

    # Automatic ARIMA forecasts -----
    expect_equal(
        WWWusage %>%
            auto.arima %>%
            forecast(h=20) %>%
            sw_sweep(fitted = F) %>%
            nrow(),
        120
    )
    expect_equal(
        WWWusage %>%
            auto.arima %>%
            forecast(h=20) %>%
            sw_sweep(fitted = T) %>%
            nrow(),
        220
    )

    # ARFIMA forecasts -----
    x <- fracdiff::fracdiff.sim(100, ma=-.4, d=.3)$series
    expect_equal(
        # Warning: no index
        arfima(x) %>%
            forecast(h=30) %>%
            sw_sweep(fitted = F) %>%
            nrow(),
        130
    )
    expect_equal(
        # Warning: no index
        arfima(x) %>%
            forecast(h=30) %>%
            sw_sweep(fitted = T) %>%
            nrow(),
        230
    )

    # STL forecasts -----
    test_sweep_2 <- USAccDeaths %>%
        stlm(modelfunction=ar) %>%
        forecast(h = 36) %>%
        sw_sweep(fitted = F)

    expect_is(test_sweep_2, "tbl")
    expect_equal(nrow(test_sweep_2), 108)
    expect_equal(ncol(test_sweep_2), 7)
    expect_equal(colnames(test_sweep_2)[[7]], "hi.95")

    test_sweep_2a <- USAccDeaths %>%
        stlm(modelfunction=ar) %>%
        forecast(h=36) %>%
        sw_sweep(fitted = T)
    expect_equal(nrow(test_sweep_2a), 180)

    # STLF -----
    test_sweep_3 <- AirPassengers %>%
        stlf(lambda=0) %>%
        sw_sweep(fitted = F)

    expect_is(test_sweep_3, "tbl")
    expect_equal(nrow(test_sweep_3), 168)
    expect_equal(ncol(test_sweep_3), 7)
    expect_equal(colnames(test_sweep_3)[[7]], "hi.95")

    test_sweep_3a <- AirPassengers %>%
        stlf(lambda=0) %>%
        sw_sweep(fitted = T)
    expect_equal(nrow(test_sweep_3a), 312)


    # STL -----
    test_sweep_4 <- USAccDeaths %>%
        stl(s.window='periodic') %>%
        forecast %>%
        sw_sweep(fitted = F)

    expect_is(test_sweep_4, "tbl")
    expect_equal(nrow(test_sweep_4), 96)
    expect_equal(ncol(test_sweep_4), 7)
    expect_equal(colnames(test_sweep_4)[[7]], "hi.95")

    test_sweep_4a <- USAccDeaths %>%
        stl(s.window='periodic') %>%
        forecast %>%
        sw_sweep(fitted = T)
    expect_equal(nrow(test_sweep_4a), 168)

    # TBATS forecast -----
    test_sweep_5 <- USAccDeaths %>%
        tbats %>%
        forecast(level = c(80, 95)) %>%
        sw_sweep(fitted = F)

    expect_is(test_sweep_5, "tbl")
    expect_equal(nrow(test_sweep_5), 96)
    expect_equal(ncol(test_sweep_5), 7)
    expect_equal(colnames(test_sweep_5)[[7]], "hi.95")

    test_sweep_5a <- USAccDeaths %>%
        tbats %>%
        forecast(level = c(80, 95)) %>%
        sw_sweep(fitted = T)
    expect_equal(nrow(test_sweep_5a), 168)

    # sweep.default() -----
    expect_warning(sw_sweep(datasets::mtcars)) # Returns original data and warning message

    # timetk index tests -----

    # Check warning if no timetk index exists
    expect_warning(
        WWWusage %>%
            auto.arima() %>%
            forecast() %>%
            sw_sweep(timetk_idx = T)
    )

    # Check integration with tk_make_future_timeseries()
    monthly_bike_sales <- bike_sales %>%
        mutate(month.date = as_date(as.yearmon(order.date))) %>%
        group_by(month.date) %>%
        summarize(total.daily.sales = sum(price.ext))

    monthly_bike_sales_ts <- tk_ts(monthly_bike_sales, start = 2011, freq = 12, silent = TRUE)

    fit <- auto.arima(monthly_bike_sales_ts)

    fcast <- forecast(fit)

    test <- sw_sweep(fcast)
    expect_equal(class(test$index), "yearmon")

    test <- sw_sweep(fcast, timetk_idx = T)
    expect_equal(class(test$index), "Date")

    test <- sw_sweep(fcast, timetk_idx = T, skip_values = ymd("2017-12-01")) %>% tail()
    expect_equal(test$index[[6]], ymd("2018-01-01"))

    test <- sw_sweep(fcast, fitted = T, timetk_idx = T, skip_values = ymd("2017-12-01"))
    expect_equal(test$index[[nrow(test)]], ymd("2018-01-01"))
})










