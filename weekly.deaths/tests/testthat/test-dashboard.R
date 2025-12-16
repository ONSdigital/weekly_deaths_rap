
test_that("deaths by cause okay for evergreen dashboard", {

  data <- data.frame(
    country = c(rep("England", 9), "Wales", rep("Nonresident", 2)),
    region = c(rep("East of England", 8), "London", "Wales",
               rep("Nonresident", 2)),
    fic10und = c(rep(c("J00", "J09", "U109", "U071"), 2), rep(NA, 4)),
    fic10men = c(rep(c("J00", "J09", "U109", "U071"), 2), rep("U099", 4)),
    week = c(rep(1, 11), 2),
    week_ending = c(rep(1, 11), 2),
    other = 1:12
  )

  expected <- tibble::tibble(
    week = c(1, 2),
    week_ending = c(1, 2),
    `England All causes` = c(9, 0),
    `England Deaths due to COVID-19 (U07.1, U07.2, U10.9)` = c(4, 0),
    `England Deaths due to diseases of the respiratory system (J00 to J99)` = c(4, 0),
    `England Deaths due to influenza or pneumonia (J09 to J18)` = c(2, 0),
    `England Deaths involving COVID-19 (U07.1, U07.2, U09.9, U10.9)` = c(5, 0),
    `England Deaths involving diseases of the respiratory system (J00 to J99)` = c(4, 0),
    `England Deaths involving influenza or pneumonia (J09 to J18)` = c(2, 0),
    `England, Wales and non-residents All causes` = c(11, 1),
    `England, Wales and non-residents Deaths due to COVID-19 (U07.1, U07.2, U10.9)` = c(4, 0),
    `England, Wales and non-residents Deaths due to diseases of the respiratory system (J00 to J99)` = c(4, 0),
    `England, Wales and non-residents Deaths due to influenza or pneumonia (J09 to J18)` = c(2, 0),
    `England, Wales and non-residents Deaths involving COVID-19 (U07.1, U07.2, U09.9, U10.9)` = c(7, 1),
    `England, Wales and non-residents Deaths involving diseases of the respiratory system (J00 to J99)` = c(4, 0),
    `England, Wales and non-residents Deaths involving influenza or pneumonia (J09 to J18)` = c(2, 0),
    `Wales All causes` = c(1, 0),
    `Wales Deaths due to COVID-19 (U07.1, U07.2, U10.9)` = c(0, 0),
    `Wales Deaths due to diseases of the respiratory system (J00 to J99)` = c(0, 0),
    `Wales Deaths due to influenza or pneumonia (J09 to J18)` = c(0, 0),
    `Wales Deaths involving COVID-19 (U07.1, U07.2, U09.9, U10.9)` = c(1, 0),
    `Wales Deaths involving diseases of the respiratory system (J00 to J99)` = c(0, 0),
    `Wales Deaths involving influenza or pneumonia (J09 to J18)` = c(0, 0)
  )

  actual <- calc_dths_cause_dashboard(data)
  expect_equal(actual, expected)
})


test_that("expected deaths okay for evergreen dashboard", {

  test_cfg <- list(year = 2025, week = 4)
  weeks <- data.frame(year = c(2024, 2024, 2025),
                      week = c(4, 5, 4),
                      week_ending = rep("some date", 3))
  exp_death_data <- data.frame(
    area = rep(c(rep("England", 1), rep("Scot", 1)), 16),
    sex = rep(c(rep("All people", 2), rep("Males", 2)), 8),
    esp_age_group = rep(c(rep("45-49", 4), rep("All ages", 4)), 4),
    dor_year = rep(c(rep(2024, 8), rep(2025, 8)), 2),
    dor_week = c(rep(4, 16), rep(5, 16)),
    extra_column = rep("something", 32),
    expected_deaths = c(rep(1, 4), 100, rep(1, 7), 100, rep(1, 7), 100,
                        rep(1, 11))
  )

  expected <- tibble::tibble(
    dor_year = c(2024, 2025, 2024),
    dor_week = c(4, 4, 5),
    `Expected deaths England` = c(100, 100, 100),
    week_ending = rep("some date", 3)
  )

  actual <- calc_exp_deaths_dashboard(exp_death_data, weeks, test_cfg)
  expect_equal(actual, expected)
})


test_that("uk deaths okay for evergreen dashboard", {

  ew_deaths <- data.frame(
    agegrp_5yr = rep("70 to 74", 10),
    agegrp_wide = rep("65 to 74", 10),
    year = rep(2023, 10),
    week = rep(1, 10),
    dor = rep(20230105, 10),
    sex = rep("Female", 10),
    country = rep("England", 10),
    place_of_death = rep("Hospital", 10)
  )
  scot_deaths <- data.frame(
    agegroup_2 = rep("70 to 74", 10),
    agegrp_wide = rep("65 to 74", 10),
    year = rep(2023, 10),
    week = rep(1, 10),
    dor = rep(20230105, 10),
    sex = rep("Female", 10),
    country = rep("Scotland", 10),
    covid = rep(0, 10),
    deaths = rep(1, 10)
  )
  ni_deaths <- data.frame(
    agegroup_2 = rep("70 to 74", 10),
    agegrp_wide = rep("65 to 74", 10),
    year = rep(2023, 10),
    week = rep(1, 10),
    dor = rep(20230105, 10),
    sex = rep("Female", 10),
    country = rep("Northern Ireland", 10),
    deaths = rep(1, 10)
  )
  test_cfg <- list(chart_years = 2023:2025, week = 8, scope = "UK", year = 2025)

  expected <- tibble::tibble(year = c(2023), week = c(1), UK = c(30))

  actual <- calc_uk_deaths_dashboard(
    ew_deaths, scot_deaths, ni_deaths, test_cfg) %>%
    dplyr::ungroup()
  expect_equal(actual, expected)
})


test_that("formatting okay for evergreen dashboard", {

  test_data <- c(88.88, 22.22, 55.5, 10, 1234.123456)
  data <- data.frame(
    dor_week = c(5, 1, 3, 2, 4),
    week_ending = c("3 February 2023", "6 January 2023", "20 January 2023",
                    "13 January 2023", "27 January 2023"),
    `England, Wales and non-residents All causes` = test_data,
    `Expected deaths England, Wales and non-residents` = test_data,
    year = rep(2023, 5),
    `England All causes` = test_data,
    `Wales All causes` = test_data,
    `England, Wales and non-residents Deaths due to COVID-19 (U07.1, U07.2, U10.9)` = test_data,
    `England, Wales and non-residents Deaths involving COVID-19 (U07.1, U07.2, U09.9, U10.9)` = test_data,
    `England, Wales and non-residents Deaths due to influenza or pneumonia (J09 to J18)` = test_data,
    `England, Wales and non-residents Deaths involving influenza or pneumonia (J09 to J18)` = test_data,
    `Expected deaths England` = test_data,
    `Expected deaths Wales` = test_data,
    UK = test_data,
    extra_column = test_data,
    check.names = FALSE
  )
  test_cfg <- list(publication_date = "11 March 2025")
  expected_data <- c(22.22, 10, 55.5, 1234.123456, 88.88)
  rounded_expected_data <- c(22.22, 10, 55.5, 1234.1235, 88.88)
  expected <- data.frame(
    publication_date = rep(as.Date("2025-03-11", format = "%Y-%m-%d"), 5),
    year = rep(2023, 5),
    week_num = 1:5,
    date = as.Date(c("2023-01-06", "2023-01-13", "2023-01-20", "2023-01-27",
                     "2023-02-03"), format = "%Y-%m-%d"),
    num_uk = expected_data,
    num_england_wales = expected_data,
    exp_england_wales = rounded_expected_data,
    num_england = expected_data,
    num_wales = expected_data,
    exp_england = rounded_expected_data,
    exp_wales = rounded_expected_data,
    covid_due_to = expected_data,
    covid_with = expected_data,
    flu_due_to = expected_data,
    flu_with = expected_data,
    check.names = FALSE
  )

  actual <- format_dashboard(data, test_cfg, round_to = 4)
  expect_equal(actual, expected)
})
