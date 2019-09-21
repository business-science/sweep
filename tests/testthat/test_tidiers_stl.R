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
    expect_equal(ncol(test), 6)

    # stlm() ----
    fit_stlm <- USAccDeaths %>%
        stlm(modelfunction=ar)

    # sw_tidy_decomp ----
    test <- sw_tidy_decomp(fit_stlm)
    expect_is(test, "tbl")
    expect_equal(nrow(test), 72)
    expect_equal(ncol(test), 6)


    # timetk index ----

    # data_ts <- USAccDeaths %>%
    #     tk_tbl() %>%
    #     mutate(index = as_date(index)) %>%
    #     tk_ts(start = 1973, freq = 12, silent = TRUE)
    #
    # fit <- stl(data_ts, s.window = "periodic")
    #
    # # timetk_idx sw_tidy_decomp -----
    # test <- fit %>% sw_tidy_decomp()
    # expect_equal(class(test$index), "yearmon")

    # timekix_idx not supported with stats::stl()
    # test <- fit %>% sw_tidy_decomp(timetk_idx = T)
    # expect_equal(class(test$index), "Date")
})
