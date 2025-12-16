test_that("Add pub year and week cols for default col = dor", {

  dummy_data <- data.frame(
    dor = c(20210101, 20210102, 20211231, 20220101, 20221230),
    reg_stat_dod = c(20221231, 20231229, 20231230, 20240101, 20240601),
    col = 1:5)

  expected <- data.frame(
    dor = c(20210101, 20210102, 20211231, 20220101, 20221230),
    reg_stat_dod = c(20221231, 20231229, 20231230, 20240101, 20240601),
    col = 1:5,
    year = c(2020, rep(2021:2022, each = 2)),
    week = c(53, rep(c(1, 52), 2)),
    week_ending =
      c("1 January 2021", "8 January 2021", "31 December 2021",
        "7 January 2022", "30 December 2022"))

  expect_equal(derive_years_week_numbers(dummy_data, 2022), expected)
})

test_that("Add pub year and week cols for reg_stat_dod", {

  dummy_data <- data.frame(
    dor = c(20210101, 20210102, 20211231, 20220101, 20221230),
    reg_stat_dod = c(20221231, 20231229, 20231230, 20240101, 20240601),
    col = 1:5)

  expected <- data.frame(
    dor = c(20210101, 20210102, 20211231, 20220101, 20221230),
    reg_stat_dod = c(20221231, 20231229, 20231230, 20240101, 20240601),
    col = 1:5,
    year = c(rep(2023:2024, each = 2), 2024),
    week = c(1, 52, 1, 1, 23),
    week_ending =
      c("6 January 2023", "29 December 2023", "5 January 2024",
        "5 January 2024", "7 June 2024"))

  actual <- derive_years_week_numbers(dummy_data, 2022, "reg_stat_dod")

  expect_equal(actual, expected)
})

test_that("Check addition of columns for publication year and start year", {

  dummy_data <- data.frame(
    # Testing breakpoints between publication years
    dor = c(20210101, 20210102, 20211231, 20220101, 20221230,
            20221231, 20231229, 20231230, 20240101, 20240601, NA),
    col = 1:11)

  expected <- data.frame(
    dor = c(20210101, 20210102, 20211231, 20220101, 20221230,
            20221231, 20231229, 20231230, 20240101, 20240601, NA),
    col = 1:11,
    year_start_date = c(20191228, rep(c(20210102, 20220101, 20221231,
                                        20231230), each = 2),
                        20231230, NA),
    year = c(2020, rep(2021:2024, each = 2), 2024, NA))

  expect_equal(calc_years_start_dates(dummy_data, "dor"), expected)
})

test_that("Check ISO start date (- 2 days for thurs) for given year", {
  # Tested for the next 10 years using https://www.epochconverter.com/weeks/2024
  expect_equal(calc_start_date_for_year(2020), 20191228)
  expect_equal(calc_start_date_for_year(2021), 20210102)
  expect_equal(calc_start_date_for_year(2022), 20220101)
  expect_equal(calc_start_date_for_year(2023), 20221231)
  expect_equal(calc_start_date_for_year(2024), 20231230)
  expect_equal(calc_start_date_for_year(2025), 20241228)
  expect_equal(calc_start_date_for_year(2026), 20251227)
  expect_equal(calc_start_date_for_year(2027), 20270102)
  expect_equal(calc_start_date_for_year(2028), 20280101)
  expect_equal(calc_start_date_for_year(2029), 20281230)
  expect_equal(calc_start_date_for_year(2030), 20291229)
})

test_that("Calc year start date for edge cases for missing calendar year", {

  data <- data.frame(
    dor  = c(20181201, 20191102, 20210101, 20210505, 20220504),
    col = 1:5)

  expected <- data.frame(
    dor = c(20181201, 20191102, 20210101, 20210505, 20220504),
    col = 1:5,
    year_start_date = c(20171230, 20181229, 20191228, 20210102, 20220101),
    year = c(2018, 2019, 2020, 2021, 2022))

  expect_equal(suppressWarnings(calc_years_start_dates(data, "dor")), expected)
})

test_that("Calc year start date for dor with missing calendar year gives
          warning", {

  data <- data.frame(
    dor  = c(20181201, 20191102, 20210101, 20210505, 20220504),
    col = 1:5)

  expected <- data.frame(
    dor = c(20181201, 20191102, 20210101, 20210505, 20220504),
    col = 1:5,
    year_start_date = c(20171230, 20181229, 20191228, 20210102, 20220101),
    year = c(2018, 2019, 2020, 2021, 2022))

  expect_warning(calc_years_start_dates(data, "dor"),
                 paste("There are calendar years in 'dor' with no data.",
                       "Please investigate."))
  expect_equal(suppressWarnings(calc_years_start_dates(data, "dor")), expected)
})


test_that("Calc year start date for dod with missing calendar year doesn't give
          a warning", {

  data <- data.frame(
    dod  = c(20181201, 20191102, 20210101, 20210505, 20220504),
    col = 1:5)

  expected <- data.frame(
    dod = c(20181201, 20191102, 20210101, 20210505, 20220504),
    col = 1:5,
    year_start_date = c(20171230, 20181229, 20191228, 20210102, 20220101),
    year = c(2018, 2019, 2020, 2021, 2022))

  expect_equal(calc_years_start_dates(data, "dod"), expected)
})

test_that("Calc year start when latest dor is in following year", {

  data <- data.frame(
    dor  = c(20200211, 20210505, 20221231),
    col = 1:3)

  expected <- data.frame(
    dor = c(20200211, 20210505, 20221231),
    col = 1:3,
    year_start_date = c(20191228, 20210102, 20221231),
    year = c(2020, 2021, 2023))

  expect_equal(calc_years_start_dates(data, "dor"), expected)
})

test_that("Calc year start when first dor is in previous year", {

  data <- data.frame(
    dor  = c(20210101, 20210505, 20221010),
    col = 1:3)

  expected <- data.frame(
    dor = c(20210101, 20210505, 20221010),
    col = 1:3,
    year_start_date = c(20191228, 20210102, 20220101),
    year = c(2020, 2021, 2022))

  expect_equal(calc_years_start_dates(data, "dor"), expected)
})

test_that("max_reg_date outputted as expected", {
  cfg <- list()
  cfg$year <- 2023
  cfg$week <- 30

  actual <- derive_max_reg_date(cfg)

  expect_type(actual, "character")
  expect_equal("20230728", actual)
})

test_that("Check addition of pub week number col (drops start date col)", {

  dummy_data <- data.frame(
    dor = c(20210101, 20210102, 20211231, 20220101, 20221230,
            20221231, 20231229, 20231230, 20240101, 20240601, NA),
    col = 1:11,
    year_start_date = c(20191228, rep(c(20210102, 20220101, 20221231,
                                        20231230), each = 2),
                        20231230, NA),
    year = c(2020, rep(2021:2024, each = 2), 2024, NA))

  expected <- data.frame(
    dor = c(20210101, 20210102, 20211231, 20220101, 20221230,
            20221231, 20231229, 20231230, 20240101, 20240601, NA),
    col = 1:11,
    year = c(2020, rep(2021:2024, each = 2), 2024, NA),
    week = c(53, rep(c(1, 52), 3), 1, 1, 23, NA),
    week_ending =
      c("1 January 2021", "8 January 2021", "31 December 2021",
        "7 January 2022", "30 December 2022", "6 January 2023",
        "29 December 2023", "5 January 2024", "5 January 2024",
        "7 June 2024", NA))
  actual <- calc_week_number_week_ending(dummy_data, "dor", 2024)
  expect_equal(actual, expected)
})

test_that("Check add of pub week number col, warning for dor_week", {

  dummy_data <- data.frame(
    dor = c(20210101, 20210102, 20211231, 20220101, 20221230,
            20221231, 20231229, 20231230, 20240101, 20240601, NA),
    col = 1:11,
    dor_week = rep(1, 11),
    year_start_date = c(20191228, rep(c(20210102, 20220101, 20221231,
                                        20231230), each = 2),
                        20231230, NA),
    year = c(2020, rep(2021:2024, each = 2), 2024, NA))

  expected <- data.frame(
    dor = c(20210101, 20210102, 20211231, 20220101, 20221230,
            20221231, 20231229, 20231230, 20240101, 20240601, NA),
    col = 1:11,
    dor_week = rep(1, 11),
    year = c(2020, rep(2021:2024, each = 2), 2024, NA),
    week = c(53, rep(c(1, 52), 3), 1, 1, 23, NA),
    week_ending =
      c("1 January 2021", "8 January 2021", "31 December 2021",
        "7 January 2022", "30 December 2022", "6 January 2023",
        "29 December 2023", "5 January 2024", "5 January 2024",
        "7 June 2024", NA))

  expect_equal(suppressWarnings(
    calc_week_number_week_ending(dummy_data, "dor", 2024)), expected)
  expect_warning(calc_week_number_week_ending(dummy_data, "dor", 2024))
  expect_warning(calc_week_number_week_ending(dummy_data, "dor", 2023))
  expect_warning(calc_week_number_week_ending(dummy_data, "dor", 2022))
  expect_warning(calc_week_number_week_ending(dummy_data, "dor", 2021))
  expect_warning(calc_week_number_week_ending(dummy_data, "dor", 2020))
})

test_that("Check year end date (ISO - 2 days for thurs) for given year", {
  expect_equal(calc_year_end_date(2020), 20210101)
  expect_equal(calc_year_end_date(2021), 20211231)
  expect_equal(calc_year_end_date(2022), 20221230)
  expect_equal(calc_year_end_date(2023), 20231229)
  expect_equal(calc_year_end_date(2024), 20241227)
  expect_equal(calc_year_end_date(2025), 20251226)
  expect_equal(calc_year_end_date(2026), 20270101)
  expect_equal(calc_year_end_date(2027), 20271231)
  expect_equal(calc_year_end_date(2028), 20281229)
  expect_equal(calc_year_end_date(2029), 20291228)
  expect_equal(calc_year_end_date(2030), 20301227)
})

test_that("Check correct number of ISO weeks in a year", {
  expect_equal(calc_weeks_in_year(2020), 53)
  expect_equal(calc_weeks_in_year(2021), 52)
  expect_equal(calc_weeks_in_year(2022), 52)
  expect_equal(calc_weeks_in_year(2023), 52)
  expect_equal(calc_weeks_in_year(2024), 52)
  expect_equal(calc_weeks_in_year(2025), 52)
  expect_equal(calc_weeks_in_year(2026), 53)
  expect_equal(calc_weeks_in_year(2027), 52)
  expect_equal(calc_weeks_in_year(2028), 52)
  expect_equal(calc_weeks_in_year(2029), 52)
  expect_equal(calc_weeks_in_year(2030), 52)
  expect_equal(calc_weeks_in_year(2031), 52)
  expect_equal(calc_weeks_in_year(2032), 53)
})

test_that("Check calc_date_one_week_before works correctly", {
  expect_equal(calc_date_one_week_before("20240108"), "20240101")
  expect_equal(calc_date_one_week_before("20240101"), "20231225")
  expect_equal(calc_date_one_week_before("20240301"), "20240223")
  expect_equal(calc_date_one_week_before("20230301"), "20230222")
})
