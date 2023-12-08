# FUNCTION: sw_*.bats -----
test_that("sw_*.bats test returns tibble with correct rows and columns.", {

    # bats()  ----
    fit_bats <- WWWusage %>%
        forecast::bats()

    # sw_tidy ----
    test <- sw_tidy(fit_bats)
    expect_s3_class(test, "tbl")
    expect_equal(nrow(test), 7)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_bats)
    expect_s3_class(test, "tbl")
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    test <- sw_augment(fit_bats, rename_index = "date")
    expect_s3_class(test, "tbl")
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 4)
    expect_equal(colnames(test)[[1]], "date")

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_bats)
    expect_s3_class(test, "tbl")
    expect_equal(nrow(test), 100)
    expect_equal(ncol(test), 3)


    # tbats()  ----
    fit_tbats <- USAccDeaths %>%
        forecast::tbats()

    # sw_tidy ----
    test <- sw_tidy(fit_tbats)
    expect_s3_class(test, "tbl")
    expect_equal(nrow(test), 8)
    expect_equal(ncol(test), 2)

    # sw_glance ----
    test <- sw_glance(fit_tbats)
    expect_s3_class(test, "tbl")
    expect_equal(nrow(test), 1)
    expect_equal(ncol(test), 12)

    # sw_augment ----
    test <- sw_augment(fit_tbats, rename_index = "date")
    expect_s3_class(test, "tbl")
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 4)
    expect_equal(colnames(test)[[1]], "date")

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_tbats)
    expect_s3_class(test, "tbl")
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 5)

    # timetk index tests -----

    # Check warning if no timetk index exists
    expect_warning(
        WWWusage %>%
            forecast::bats() %>%
            sw_augment(timetk_idx = TRUE)
    )

    monthly_bike_sales <- bike_sales %>%
        dplyr::mutate(month.date = lubridate::as_date(zoo::as.yearmon(order.date))) %>%
        dplyr::group_by(month.date) %>%
        dplyr::summarize(total.daily.sales = sum(price.ext))

    monthly_bike_sales_ts <- tk_ts(monthly_bike_sales, start = 2011, frequency = 12, silent = TRUE)

    # BATS timetk index ----

    fit <- forecast::bats(monthly_bike_sales_ts)

    # timetk_idx sw_augment ----
    test <- fit %>% sw_augment()
    expect_s3_class(test$index, "yearmon")

    test <- fit %>% sw_augment(timetk_idx = TRUE)
    expect_s3_class(test$index, "Date")

    # timetk_idx sw_tidy_decomp -----
    test <- fit %>% sw_tidy_decomp()
    expect_s3_class(test$index, "yearmon")

    test <- fit %>% sw_tidy_decomp(timetk_idx = T)
    expect_s3_class(test$index, "Date")

    # TBATS timetk index ----

    data_ts <- USAccDeaths %>%
        tk_tbl() %>%
        dplyr::mutate(index = lubridate::as_date(index)) %>%
        tk_ts(start = 1973, frequency = 12, silent = TRUE)

    fit <- forecast::tbats(data_ts)

    # timetk_idx sw_augment ----
    test <- fit %>% sw_augment()
    expect_s3_class(test$index, "yearmon")

    test <- fit %>% sw_augment(timetk_idx = TRUE)
    expect_s3_class(test$index, "Date")

    # timetk_idx sw_tidy_decomp -----
    test <- fit %>% sw_tidy_decomp()
    expect_s3_class(test$index, "yearmon")

    test <- fit %>% sw_tidy_decomp(timetk_idx = TRUE)
    expect_s3_class(test$index, "Date")

})
