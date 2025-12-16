test_that("Delay table calculation", {

  data <- data.frame(
    a = 1:14,
    week = c(rep(45, 6), rep(46, 8)),
    week_ending = c(rep("23 October 2023", 6), rep("31 October 2023", 8)),
    dor = c("20231204", "20231205", "20231206", "20231206", "20231206",
            "20231206", rep("20231206", 8)),
    dod = c("20231204", "20231128", "20231127", "20231204", "20231205",
            "20210617", rep("20231205", 4), rep("20231204", 4))
  )

  expected <- data.frame(
    week = c(45, 46),
    week_ending = c("23 October 2023", "31 October 2023"),
    total_deaths = c(6, 8),
    delay_within_7 = c(3, 8),
    percentage_within_7 = c(50, 100),
    median_delay = c(4.5, 1.5),
    q1 = c(1.3, 1),
    q3 = c(8.5, 2)
  )

  class(expected) <- c("tbl_df", "tbl", "data.frame")

  actual <- create_delay_summary(data, c("week", "week_ending"))

  expect_equal(actual, expected)
})

test_that("Delay table by certification is calculated correctly", {

  data <- data.frame(
    a = 1:14,
    week = c(rep(45, 6), rep(46, 8)),
    week_ending = c(rep("23 October 2023", 6), rep("31 October 2023", 8)),
    dor = c("20231204", "20231205", "20231206", "20231206", "20231206",
            "20231206", rep("20231206", 8)),
    dod = c("20231204", "20231128", "20231127", "20231204", "20231205",
            "20210617", rep("20231205", 4), rep("20231204", 4)),
    cert_type = c(rep("cert_1", 3), rep("cert_2", 3), rep("cert_1", 4),
                  rep("cert_2", 4)),
    cert_type_agg = c(rep("cert_1", 3), rep("cert_2", 2),
                      "Certified by coroner", rep("cert_1", 4),
                      rep("cert_2", 3), "Certified by coroner"))

  expected <- data.frame(
    week = c(45, 46, rep(45, 2), rep(46, 2), 45, 46),
    week_ending = c("23 October 2023", "31 October 2023",
                    rep("23 October 2023", 2), rep("31 October 2023", 2),
                    "23 October 2023", "31 October 2023"),
    total_deaths = c(6, 8, 3, 3, 4, 4, 1, 1),
    delay_within_7 = c(3, 8, 1, 2, 4, 4, 0, 1),
    percentage_within_7 = c(50, 100, 33.3, 66.7, 100, 100, 0, 100),
    median_delay = c(4.5, 1.5, 7, 2, 1, 2, 902, 2),
    q1 = c(1.3, 1, 3.5, 1.5, 1, 2, 902, 2),
    q3 = c(8.5, 2, 8, 452, 1, 2, 902, 2),
    cert_type = c(rep("All deaths", 2), "cert_1", "cert_2", "cert_1", "cert_2",
                  "Certified by coroner", "Certified by coroner")
  )

  class(expected) <- c("tbl_df", "tbl", "data.frame")

  actual <- create_delay_summary_certtype(data)

  expect_equal(actual, expected)
})

test_that("Calculate delay days", {

  data <- data.frame(
    a = 1:5,
    dor = c("20231204", "20231205", "20231206", "20231206", "20231206"),
    dod = c("20231204", "20231128", "20231127", "20231204", "20210617")
  )

  expected <- data.frame(
    a = 1:5,
    dor = as.Date(
      c("2023-12-04", "2023-12-05", "2023-12-06", "2023-12-06", "2023-12-06"),
      format = "%Y-%m-%d"),
    dod = as.Date(
      c("2023-12-04", "2023-11-28", "2023-11-27", "2023-12-04", "2021-06-17"),
      format = "%Y-%m-%d"),
    delay_days = c(0, 7, 9, 2, 902)
  )

  actual <- calc_registration_delays(data)
  expect_equal(expected, actual)
})

test_that("Error if registration before occurence", {

  data <- data.frame(
    a = 1:2,
    dor = c("20231203", "20231205"),
    dod = c("20231204", "20231128")
  )

  expect_error(
    calc_registration_delays(data),
    "A death registration date is before death occurence date")
})

test_that("Format delay summary table", {

  data <- data.frame(
    week = c(1, 2, 3, 4, 5),
    total_deaths = rep(1, 5),
    delay_within_7 = rep(1, 5),
    percentage_within_7 = rep(1, 5),
    median_delay = rep(1, 5),
    q1 = rep(1, 5),
    q3 = rep(1, 5),
    week_ending = rep("5 December 2023", 5)
  )

  expected <- data.frame(
    `Week number` = c(5, 4, 3, 2, 1),
    `Week ending` = rep("5 December 2023", 5),
    `Deaths registered in week` = rep(1, 5),
    `Number of deaths that occurred within previous 7 days (delay 0 to 6 days)`
    = rep(1, 5),
    tmp_col = rep(1, 5),
    `Median time from death to registration (days)` = rep(1, 5),
    `25th percentile of time from death to registration (days)` = rep(1, 5),
    `75th percentile of time from death to registration (days)` = rep(1, 5),
    check.names = FALSE)

  colnames(expected)[5] <-
    paste("Percentage of deaths that occurred within previous 7 days (delay 0",
          "to 6 days)")

  expect_equal(format_delay_summary(data), expected)
})

test_that("Adding zero counts to delays data", {

  data <- data.frame(
    week = c(1, 1, 1, 2),
    week_ending = c("X", "X", "X", "Y"),
    total_deaths = rep(100, 4),
    delay_within_7 = rep(50, 4),
    percentage_within_7 = rep(25.0, 4),
    median_delay = rep(5.5, 4),
    q1 = rep(1.0, 4),
    q3 = rep(14.5, 4),
    place_of_death = factor(rep("place 1", 4),
                            c("All places", "place 1", "place 2")),
    cert_type = factor(c("cert 1", "All certs", "cert 2", "cert 1"),
                       c("All certs", "cert 1", "cert 2")),
    area = factor(rep("England", 4), c("England"))
  )

  expected <- tibble::tibble(
    week = rep(1:2, each = 9),
    week_ending = rep(c("X", "Y"), each = 9),
    cert_type = factor(
      rep(rep(c("All certs", "cert 1", "cert 2"), each = 3), 2),
      c("All certs", "cert 1", "cert 2")),
    place_of_death = factor(
      rep(rep(c("All places", "place 1", "place 2"), 3), 2),
      c("All places", "place 1", "place 2")),
    area = factor(rep("England", 18), "England"),
    total_deaths = c(0, 100, 0, 0, 100, 0, 0, 100, rep(0, 5), 100, rep(0, 4)),
    delay_within_7 = c(NA, 50, NA, NA, 50, NA, NA, 50,
                       rep(NA, 5), 50, rep(NA, 4)),
    percentage_within_7 = c(NA, 25.0, NA, NA, 25.0, NA, NA,
                            25.0, rep(NA, 5), 25.0, rep(NA, 4)),
    median_delay = c(NA, 5.5, NA, NA, 5.5, NA, NA, 5.5,
                     rep(NA, 5), 5.5, rep(NA, 4)),
    q1 = c(NA, 1.0, NA, NA, 1.0, NA, NA, 1.0,
           rep(NA, 5), 1.0, rep(NA, 4)),
    q3 = c(NA, 14.5, NA, NA, 14.5, NA, NA, 14.5,
           rep(NA, 5), 14.5, rep(NA, 4))
  )

  expect_equal(add_zero_counts_delays(data), expected)
})

test_that("Creating summary of delays by region/cert_type/pod", {

  data <- data.frame(
    dor = rep(20250103, 9),
    dod = rep(20250103, 9),
    week = rep(1, 9),
    week_ending = rep("3 January 2025", 9),
    cert_type = c("Unknown/Uncertified", "cert 1", "cert 1", rep("cert 2", 6)),
    region = c(rep("East of England", 7), "Wales", "Nonresident"),
    place_of_death = rep("Hospital", 9)
  )

  expected <- data.frame(
    week = rep(1, 8),
    week_ending = rep("3 January 2025", 8),
    cert_type = c(NA, NA, rep(c("Unknown/Uncertified", "cert 1", "cert 2"), 2)),
    total_deaths = c(7, 7, 1, 2, 4, 1, 2, 4),
    delay_within_7 = c(7, 7, 1, 2, 4, 1, 2, 4),
    percentage_within_7 = rep(100, 8),
    median_delay = rep(0, 8),
    q1 = rep(0, 8),
    q3 = rep(0, 8),
    place_of_death = c(NA, "Hospital", NA, NA, NA, rep("Hospital", 3)),
    area = rep("East of England", 8)
  )

  class(expected) <- c("tbl_df", "tbl", "data.frame")

  expect_equal(create_area_cert_pod_delay(data, "region", "cert_type"),
               expected)
})

test_that("Creating summary of delays by region/cert_type/pod, pre 2025", {

  data <- data.frame(
    dor = rep(20241218, 9),
    dod = rep(20241218, 9),
    week = rep(51, 9),
    week_ending = rep("20 December 2024", 9),
    cert_type = c("Uncertified", "Unknown", "cert 1", rep("cert 2", 6)),
    region = c(rep("East of England", 7), "Wales", "Nonresident"),
    place_of_death = rep("Hospital", 9)
  )

  expected <- data.frame(
    week = rep(51, 10),
    week_ending = rep("20 December 2024", 10),
    cert_type = c(
      NA, NA, rep(c("Uncertified", "Unknown", "cert 1", "cert 2"), 2)),
    total_deaths = c(7, 7, 1, 1, 1, 4, 1, 1, 1, 4),
    delay_within_7 = c(7, 7, 1, 1, 1, 4, 1, 1, 1, 4),
    percentage_within_7 = rep(100, 10),
    median_delay = rep(0, 10),
    q1 = rep(0, 10),
    q3 = rep(0, 10),
    place_of_death = c(NA, "Hospital", NA, NA, NA, NA, rep("Hospital", 4)),
    area = rep("East of England", 10)
  )

  class(expected) <- c("tbl_df", "tbl", "data.frame")

  expect_equal(create_area_cert_pod_delay(data, "region", "cert_type"),
               expected)
})

test_that("Creating summary of delays by country/cert_type_agg/pod", {

  data <- data.frame(
    dor = rep(20250103, 9),
    dod = rep(20250103, 9),
    week = rep(1, 9),
    week_ending = rep("3 January 2025", 9),
    cert_type_agg = c("Uncertified", "Unknown", rep("Certified by coroner", 7)),
    region = c(rep("East of England", 7), "Wales", "Nonresident"),
    place_of_death = rep("Hospital", 9),
    country = c(rep("England", 7), "Wales", "Nonresident")
  )

  expected <- data.frame(
    week = rep(1, 8),
    week_ending = rep("3 January 2025", 8),
    cert_type = rep("Certified by coroner", 8),
    total_deaths = c(7, 7, 5, 1, 1, 5, 1, 1),
    delay_within_7 = c(7, 7, 5, 1, 1, 5, 1, 1),
    percentage_within_7 = rep(100, 8),
    median_delay = rep(0, 8),
    q1 = rep(0, 8),
    q3 = rep(0, 8),
    place_of_death = c(NA, "Hospital", NA, NA, NA, rep("Hospital", 3)),
    area = c(NA, NA, rep(c("England", "Nonresident", "Wales"), 2))
  )

  class(expected) <- c("tbl_df", "tbl", "data.frame")

  expect_equal(
    create_area_cert_pod_delay(data, "country", "cert_type_agg"),
    expected)
})

test_that("Errors are given for incorrect argument inputs", {

  data <- data.frame(
    dor = rep(20250103, 9),
    dod = rep(20250103, 9),
    week = rep(1, 9),
    week_ending = rep("3 January 2025", 9),
    cert_type_agg = c("Uncertified", "Unknown", rep("Certified by coroner", 7)),
    region = c(rep("East of England", 7), "Wales", "Nonresident"),
    place_of_death = rep("Hospital", 9),
    country = c(rep("England", 7), "Wales", "Nonresident")
  )

  expect_error(create_area_cert_pod_delay(data, "c", "cert_type_agg"),
               'area_col argument must be "region" or "country"')

  expect_error(create_area_cert_pod_delay(data, "region", "cert"),
               'cert_col argument must be "cert_type" or "cert_type_agg"')

})
