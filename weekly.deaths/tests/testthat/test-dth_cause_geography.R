test_that("Deaths counted for all and individual causes", {

  test_data <- data.frame(
    country = c(rep("England", 9), "Wales", rep("Nonresident", 2)),
    region = c(rep("East of England", 8), "London", "Wales",
               rep("Nonresident", 2)),
    fic10und = c(rep(c("J00", "J09", "U109", "U071"), 2), rep(NA, 4)),
    fic10men = c(rep(c("J00", "J09", "U109", "U071"), 2), rep("U099", 4)),
    week = c(rep(1, 11), 2),
    week_ending = c(rep(1, 11), 2),
    other = 1:12
  )

  actual <- count_dths_cause_geo(test_data)

  causes <- c(
    "All causes",
    "Deaths due to COVID-19 (U07.1, U07.2, U10.9)",
    "Deaths due to diseases of the respiratory system (J00 to J99)",
    "Deaths due to influenza or pneumonia (J09 to J18)",
    "Deaths involving COVID-19 (U07.1, U07.2, U09.9, U10.9)",
    "Deaths involving diseases of the respiratory system (J00 to J99)",
    "Deaths involving influenza or pneumonia (J09 to J18)")

  expected <- tibble::tibble(
    week = c(rep(1, 35), rep(2, 35)),
    week_ending = c(rep(1, 35), rep(2, 35)),
    region = c(
      rep("East of England", 7),
      rep("England", 7),
      rep("England, Wales and non-residents", 7),
      rep("London", 7),
      rep("Wales", 7),
      rep("East of England", 7),
      rep("England", 7),
      rep("England, Wales and non-residents", 7),
      rep("London", 7),
      rep("Wales", 7)),
    cause = c(rep(causes, 10)),
    n = c(8, 4, 4, 2, 4, 4, 2, 9, 4, 4, 2, 5, 4, 2, 11, 4, 4, 2, 7, 4, 2,
          1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, rep(0, 16), 1, 0, 0, 0, 1,
          rep(0, 16))
  )

  expect_equal(actual, expected)
})

test_that("Data mulitplied by region as expected", {

  test_data <- data.frame(
    country = c("England", "Wales", "England", "Wales", "England", "Wales",
                rep("Nonresident", 3), "England"),
    region = c("East of England", "Wales", "London", "Wales", "South East",
               "Wales", rep("Nonresident", 3), "Yorkshire and the Humber"),
    another = c("the", "end", "is", "never", "the", "end", "is", "never", "the",
                "end")
  )

  actual <- prepare_regions_for_analysis(test_data)

  expected <- data.frame(
    country = c(rep("England, Wales and non-residents", 10), rep("England", 5),
                "Wales", "England", "Wales", "England", "Wales", "England"),
    region = c(rep("England, Wales and non-residents", 10), rep("England", 4),
               "East of England", "Wales", "London", "Wales", "South East",
               "Wales", "Yorkshire and the Humber"),
    another = c("the", "end", "is", "never", "the", "end", "is", "never", "the",
                "end", "the", "is", "the", "end", "the", "end", "is", "never",
                "the", "end", "end")
  )

  expect_equal(actual, expected)
})

test_that("Deaths counted by cause and region", {

  test_data <- data.frame(
    week = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 5, 5),
    week_ending = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 5, 5),
    region = c("England", "Wales", "England", "Wales", "England", "Wales",
               "England", "Wales", "England", "Wales", "Wales", "England"),
    resp_inv = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE,
                 FALSE, FALSE, FALSE),
    resp_due = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
                 FALSE, FALSE, FALSE),
    infl_inv = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE,
                 FALSE, FALSE, FALSE),
    infl_due = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
                 FALSE, FALSE, FALSE),
    covid_inv = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE,
                  FALSE, FALSE, FALSE),
    covid_due = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
                  TRUE, TRUE, FALSE),
    unrelated = 1:12
  )

  expected <- data.frame(
    week = c(rep(c(1, 3, 4, 4, 1, 3, 4), 3), 5),
    week_ending = c(rep(c(1, 3, 4, 4, 1, 3, 4), 3), 5),
    region = c(rep(c(rep("England", 3), rep(c("Wales", "England"), 2)), 3),
               "Wales"),
    cause = c(
      rep("Deaths involving diseases of the respiratory system (J00 to J99)",
          4),
      rep("Deaths due to diseases of the respiratory system (J00 to J99)", 3),
      rep("Deaths involving influenza or pneumonia (J09 to J18)", 4),
      rep("Deaths due to influenza or pneumonia (J09 to J18)", 3),
      rep("Deaths involving COVID-19 (U07.1, U07.2, U09.9, U10.9)", 4),
      rep("Deaths due to COVID-19 (U07.1, U07.2, U10.9)", 4)),
    n = c(rep(1, 21), 2)
  )

  actual <- count_dths_cause_region(test_data)
  expect_equal(expected, actual)
})

test_that("Error thrown when cause column missing", {

  test_data <- data.frame(
    week = 1,
    region = "England",
    resp_inv = TRUE,
    resp_due = FALSE,
    infl_inv = TRUE,
    infl_due = TRUE,
    covid_inv = TRUE,
    unrelated = TRUE
  )

  expect_error(count_dths_cause_region(test_data))
})

test_that("Formatting for deaths by cause as expected", {

  dummy_data <- data.frame(
    week_ending = c(rep(1, 84), 2),
    week = c(rep(1, 84), 2),
    n = rep(1, 85),
    region = c(rep(
      c("East of England", "London", "South East", "South West", "North West",
        "England, Wales and non-residents", "East Midlands", "England",
        "North East", "Wales", "Yorkshire and The Humber", "West Midlands"),
      7),
      "Wales"),
    cause = c(
      rep("Deaths involving influenza or pneumonia (J09 to J18)", 12),
      rep("Deaths due to influenza or pneumonia (J09 to J18)", 12),
      rep("Deaths involving COVID-19 (U07.1, U07.2, U09.9, U10.9)", 12),
      rep("All causes", 12),
      rep("Deaths involving diseases of the respiratory system (J00 to J99)",
          12),
      rep("Deaths due to diseases of the respiratory system (J00 to J99)", 12),
      rep("Deaths due to COVID-19 (U07.1, U07.2, U10.9)", 13))
  )

  lvls <- create_factor_levels()

  expected <- data.frame(
    `Week number` = c(2, rep(1, 84)),
    `Week ending` = c(2, rep(1, 84)),
    `Area of usual residence` = factor(
      c("Wales", rep("England, Wales and non-residents", 7), rep("England", 7),
        rep("Wales", 7), rep("North East", 7), rep("North West", 7),
        rep("Yorkshire and The Humber", 7), rep("East Midlands", 7),
        rep("West Midlands", 7), rep("East of England", 7), rep("London", 7),
        rep("South East", 7), rep("South West", 7)),
      levels = lvls$country_region),
    `Cause of death` = factor(
      c("Deaths due to COVID-19 (U07.1, U07.2, U10.9)",
      rep(lvls$causes, 12)),
      levels = lvls$causes),
    `Number of deaths` = rep(1, 85),
    check.names = FALSE
  )

  actual <- format_dths_cause_geo(dummy_data)
  expect_equal(actual, expected)
})

test_that("Missing observations have been replaced with zero counts", {

  df_test <- data.frame(
    week = c(rep(1, 84), 2),
    week_ending = c(rep(1, 84), 2),
    region = c(rep(
      c("East of England", "London", "South East", "South West", "North West",
        "England, Wales and non-residents", "East Midlands", "England",
        "North East", "Wales", "Yorkshire and The Humber", "West Midlands"),
      7),
      "Wales"),
    cause = c(
      rep("Deaths involving influenza or pneumonia (J09 to J18)", 12),
      rep("Deaths due to influenza or pneumonia (J09 to J18)", 12),
      rep("Deaths involving COVID-19 (U07.1, U07.2, U09.9, U10.9)", 12),
      rep("All causes", 12),
      rep("Deaths involving diseases of the respiratory system (J00 to J99)",
          12),
      rep("Deaths due to diseases of the respiratory system (J00 to J99)", 12),
      rep("Deaths due to COVID-19 (U07.1, U07.2, U10.9)", 13)),
    n = rep(1, 85)
  )


  actual <- registration_cause_zero_values(df_test)


  lvls <- create_factor_levels()

  expected <- tibble::tibble(
    week = c(rep(1, 84), rep(2, 84)),
    week_ending = c(rep(1, 84), rep(2, 84)),
    region = c(rep(
      c("East of England", "London", "South East", "South West", "North West",
        "England, Wales and non-residents", "East Midlands", "England",
        "North East", "Wales", "Yorkshire and The Humber", "West Midlands"),
      7),
      "Wales",
      rep(
        c("East of England", "London", "South East", "South West", "North West",
          "England, Wales and non-residents", "East Midlands", "England",
          "North East", "Wales", "Yorkshire and The Humber", "West Midlands"),
        6),
      c("East of England", "London", "South East", "South West", "North West",
        "England, Wales and non-residents", "East Midlands", "England",
        "North East", "Yorkshire and The Humber", "West Midlands")),
    cause = c(
      rep("Deaths involving influenza or pneumonia (J09 to J18)", 12),
      rep("Deaths due to influenza or pneumonia (J09 to J18)", 12),
      rep("Deaths involving COVID-19 (U07.1, U07.2, U09.9, U10.9)", 12),
      rep("All causes", 12),
      rep("Deaths involving diseases of the respiratory system (J00 to J99)",
          12),
      rep("Deaths due to diseases of the respiratory system (J00 to J99)", 12),
      rep("Deaths due to COVID-19 (U07.1, U07.2, U10.9)", 13),
      rep("Deaths involving influenza or pneumonia (J09 to J18)", 12),
      rep("Deaths due to influenza or pneumonia (J09 to J18)", 12),
      rep("Deaths involving COVID-19 (U07.1, U07.2, U09.9, U10.9)", 12),
      rep("All causes", 12),
      rep("Deaths involving diseases of the respiratory system (J00 to J99)",
          12),
      rep("Deaths due to diseases of the respiratory system (J00 to J99)", 12),
      rep("Deaths due to COVID-19 (U07.1, U07.2, U10.9)", 11)),
    n = c(rep(1, 85), rep(0, 83))) %>%
    dplyr::arrange(week, week_ending, region, cause)
  expect_equal(actual, expected)

})
