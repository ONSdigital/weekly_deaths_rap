# Prevents scientific notation of numerics (i.e. standard form)
options("scipen" = 999)


test_that("Required columns are present in data", {
  expect_error(
    check_columns_present(data.frame(a = 1, b = 2), c("a", "b", "c")),
    "Missing required columns: c.")
})

##### Generate report #####

test_that("Report generated correctly, no fails", {
  data <- data.frame(
    dor = 20230101,
    reg_stat_dod = 20230101,
    ageinyrs = 20,
    agegroup_wide = "15-44",
    agegroup_2 = "20-24",
    ctrynm = "England",
    dor_week = 10,
    pod = "Home",
    region = "London",
    sex = 1,
    certtype = 1,
    fic10und = "E100",
    fic10men1 = "E100",
    fic10men2 = "E100",
    fic10men3 = "E100",
    fic10men4 = "E100",
    fic10men5 = "E100",
    fic10men6 = "E100",
    fic10men7 = "E100",
    fic10men8 = "E100",
    fic10men9 = "E100",
    fic10men10 = "E100",
    fic10men11 = "E100",
    fic10men12 = "E100",
    fic10men13 = "E100",
    fic10men14 = "E100",
    fic10men15 = "E100",
    other = 1)

  report <- list(
    pass_count = 1,
    fail_count = 0,
    null_count = 0,
    fail_indices = numeric(),
    null_indices = numeric())

  expected <- list(
    dor = report,
    dod = report,
    ageinyrs = report,
    agegroup_wide = report,
    agegroup = report,
    country = report,
    dor_week = report,
    place_of_death = report,
    region = report,
    sex = report,
    certtype = report,
    fic10und = report,
    fic10men1 = report,
    fic10men2 = report,
    fic10men3 = report,
    fic10men4 = report,
    fic10men5 = report,
    fic10men6 = report,
    fic10men7 = report,
    fic10men8 = report,
    fic10men9 = report,
    fic10men10 = report,
    fic10men11 = report,
    fic10men12 = report,
    fic10men13 = report,
    fic10men14 = report,
    fic10men15 = report
  )

  expect_equal(names(expected), names(validate_data(data)))
  expect_equal(expected, validate_data(data), ignore_attr = TRUE)
})

test_that("Report generated correctly, no fails", {
  data <- data.frame(
    sex = c(1, 2, 2, NA),
    other = 1:4)
  expected <- list(
    pass_count = 4,
    fail_count = 0,
    null_count = 1,
    fail_indices = numeric(),
    null_indices = 4
  )
  actual <- generate_report(data$sex, is_valid_sex)
  expect_equal(expected, actual)
})

test_that("Report generated correctly, with fails, expect warning", {
  data <- data.frame(
    sex = c(1:3, NA),
    other = 1:4)
  expected <- list(
    pass_count = 3,
    fail_count = 1,
    null_count = 1,
    fail_indices = 3,
    null_indices = 4)

  # Warning suppressed in the expect equal call as we test for the warning in
  # next test
  expect_equal(expected,
               suppressWarnings(generate_report(data$sex, is_valid_sex)))
  expect_warning(generate_report(data$sex, is_valid_sex),
                 paste("There are 1 records that fail validation for",
                       "is_valid_sex. Check validation report."))
})

##### Date validation #####

test_that("Is valid date column, string above default min or NA, default max", {
  data <- data.frame(date = c("19000000", NA), other = 1:2)
  expect_equal(c(TRUE, TRUE), is_valid_date_format(data$date))
})

test_that("Valid date col, num > default min, non-stand form, default max", {
  data <- data.frame(
    # If the option preventing standard form is not on, this test fails
    date = c(19000000, 19000001),
    other = 1:2)
  expect_equal(c(TRUE, TRUE), is_valid_date_format(data$date))
})

test_that("Is valid date column, numeric above default min/NA, default max", {
  data <- data.frame(date = c(19000001, NA), other = 1:2)
  expect_equal(c(TRUE, TRUE), is_valid_date_format(data$date))
})

test_that("Is invalid date column, string below default min, default max", {
  data <- data.frame(date = "18999999", other = 1)
  expect_equal(FALSE, is_valid_date_format(data$date))
})

test_that("Is invalid date column, string below chosen min, default max", {
  data <- data.frame(date = "12345677", other = 1)
  expect_equal(FALSE, is_valid_date_format(data$date, min = "12345678"))
})

test_that("Is invalid date column, string above max", {
  data <- data.frame(date = "99999999", other = 1)
  expect_equal(FALSE, is_valid_date_format(data$date, max = "20250000"))
})

test_that("Is invalid date column, letter in string, default max", {
  data <- data.frame(date = "1234567L", other = 1)
  expect_equal(FALSE, is_valid_date_format(data$date))
})

test_that("Is invalid date column, too short numeric, default max", {
  data <- data.frame(date = 1, other = 1)
  expect_equal(FALSE, is_valid_date_format(data$date))
})

test_that("Is invalid date column, too long numeric, default max", {
  data <- data.frame(date = 123456789, other = 1)
  expect_equal(FALSE, is_valid_date_format(data$date))
})

##### String length validation #####

test_that("Is valid string length in column, default", {
  data <- data.frame(char = c("length7", NA), other = 1:2)
  expect_equal(c(TRUE, TRUE), is_valid_char_length(data$char))
})

test_that("Is invalid string length in column, default", {
  data <- data.frame(char = "length_wrong", other = 1)
  expect_equal(FALSE, is_valid_char_length(data$char))
})

test_that("Is valid string length in column, chosen length", {
  data <- data.frame(char = "lensix", other = 1)
  expect_equal(TRUE, is_valid_char_length(data$char, 6))
})

test_that("Is invalid string length in column, chosen length", {
  data <- data.frame(char = "length_wrong", other = 1)
  expect_equal(FALSE, is_valid_char_length(data$char, 6))
})

##### Age validation #####

test_that("Is valid age, default max", {
  data <- data.frame(age = c(1, 129, NA), other = 1:3)
  expect_equal(rep(TRUE, 3), is_valid_age(data$age))
})

test_that("Is invalid age, negative age (below min), default max", {
  data <- data.frame(age = -1, other = 1)
  expect_equal(FALSE, is_valid_age(data$age))
})

test_that("Is invalid age, age over max, default max", {
  data <- data.frame(age = 131, other = 1)
  expect_equal(FALSE, is_valid_age(data$age))
})

test_that("Is valid age, chosen max", {
  data <- data.frame(age = 100, other = 1)
  expect_equal(TRUE, is_valid_age(data$age, max = 100))
})

test_that("Is invalid age, negative age (below min), chosen max", {
  data <- data.frame(age = -1, other = 1)
  expect_equal(FALSE, is_valid_age(data$age, max = 100))
})

test_that("Is invalid age, age over max, chosen max", {
  data <- data.frame(age = 101, other = 1)
  expect_equal(FALSE, is_valid_age(data$age, max = 100))
})

##### Sex validation #####

test_that("Is valid sex, numeric", {
  data <- data.frame(
    sex = c(1, 2, NA),
    other = 1:3)
  expect_equal(rep(TRUE, 3), is_valid_sex(data$sex))
})

test_that("Is valid sex, string", {
  data <- data.frame(
    sex = c("1", "2", NA),
    other = 1:3)
  expect_equal(rep(TRUE, 3), is_valid_sex(data$sex))
})

test_that("Is invalid sex", {
  data <- data.frame(
    sex = c("other", "value"),
    other = 1:2)
  expect_equal(rep(FALSE, 2), is_valid_sex(data$sex))
})

##### ICD10 validation #####

test_that("Is valid icd10", {
  data <- data.frame(
    fic10 = c("A11", "B222", "C333", NA),
    other = 1:4)
  expect_equal(rep(TRUE, 4), is_valid_icd_format(data$fic10))
})

test_that("Is invalid icd10, single letter", {
  data <- data.frame(fic10 = "A", other = 1)
  expect_equal(FALSE, is_valid_icd_format(data$fic10))
})

test_that("Is invalid icd10, 1 letter, 1 number", {
  data <- data.frame(fic10 = "A1", other = 1)
  expect_equal(FALSE, is_valid_icd_format(data$fic10))
})

test_that("Is invalid icd10, 1 number", {
  data <- data.frame(fic10 = 1, other = 1)
  expect_equal(FALSE, is_valid_icd_format(data$fic10))
})

test_that("Is invalid icd10, 3 numbers", {
  data <- data.frame(fic10 = 100, other = 1)
  expect_equal(FALSE, is_valid_icd_format(data$fic10))
})

##### Age group validation #####

test_that("Is valid 5yr agegroup", {
  data <- data.frame(
    agegrp = c("<1", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29",
               "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
               "65-69", "70-74", "75-79", "80-84", "85-89", "90+", NA),
    other = 1:21)
  expect_equal(rep(TRUE, 21), is_valid_agegrp(data$agegrp))
})

test_that("Is invalid 5yr agegroup, wrong string", {
  data <- data.frame(agegrp = "1", other = 1)
  expect_equal(FALSE, is_valid_agegrp(data$agegrp))
})

test_that("Is invalid 5yr agegroup, wrong numeric", {
  data <- data.frame(agegrp = 1, other = 1)
  expect_equal(FALSE, is_valid_agegrp(data$agegrp))
})

test_that("Is valid wide agegroup", {
  data <- data.frame(
    agegrp = c("<1", "01-14", "15-44", "45-64", "65-74", "75-84", "85+", NA),
    other = 1:8)
  expect_equal(rep(TRUE, 8), is_valid_agegrp_wide(data$agegrp))
})

test_that("Is invalid wide agegroup, wrong string", {
  data <- data.frame(agegrp = "1", other = 1)
  expect_equal(FALSE, is_valid_agegrp(data$agegrp))
})

test_that("Is invalid wide agegroup, wrong numeric", {
  data <- data.frame(agegrp = 1, other = 1)
  expect_equal(FALSE, is_valid_agegrp_wide(data$agegrp))
})

##### Country/region validation #####

test_that("Is valid combination of region and country", {
  data <- data.frame(
    region = c("East Midlands", "East of England", "London", "North East",
               "North West", "South East", "South West", "West Midlands",
               "Yorkshire and The Humber", "Wales", "Nonresident", NA),
    ctrynm = c(rep("England", 9), "Wales", NA, NA),
    other = 1:12)
  expect_error(check_region_country(data),
               NA)
})

##### Country validation #####

test_that("Is valid country", {
  data <- data.frame(
    country = c("England", "Wales", NA),
    other = 1:3)
  expect_equal(rep(TRUE, 3), is_valid_country(data$country))
})

test_that("Is invalid country, wrong string", {
  data <- data.frame(
    country = c("Scotland", "Northern Ireland"),
    other = 1:2)
  expect_equal(rep(FALSE, 2), is_valid_country(data$country))
})

##### Region validation #####

test_that("Is valid region", {
  data <- data.frame(
    region = c("East Midlands", "East of England", "London", "Nonresident",
               "North East", "North West", "South East", "South West", "Wales",
               "West Midlands", "Yorkshire and The Humber", NA),
    other = 1:12)
  expect_equal(rep(TRUE, 12), is_valid_region(data$region))
})

test_that("Is invalid region, wrong string", {
  data <- data.frame(
    region = c("wrong", "string"),
    other = 1:2)
  expect_equal(rep(FALSE, 2), is_valid_region(data$region))
})

##### Week number validation #####

test_that("Is valid week number", {
  data <- data.frame(
    week_no = c(1:53, NA, -53),
    other = 1:55)
  expect_equal(rep(TRUE, 55), is_valid_week_num(data$week_no))
})

test_that("Is invalid week number, wrong number", {
  expect_equal(FALSE, is_valid_week_num(-54))
  expect_equal(FALSE, is_valid_week_num(54))
})

test_that("Is invalid week number, string", {
  data <- data.frame(week_no = "string", other = 1)
  expect_equal(FALSE, is_valid_week_num(data$week_no))
})

##### Place of death validation #####

test_that("Is valid place of death", {
  data <- data.frame(
    pod = c("Care home", "Elsewhere", "Home", "Hospice", "Hospital",
            "Other communal establishment", NA),
    other = 1:7)
  expect_equal(rep(TRUE, 7), is_valid_pod(data$pod))
})

test_that("Is invalid place of death, numeric", {
  data <- data.frame(pod = 0, other = 1)
  expect_equal(FALSE, is_valid_pod(data$pod))
})

test_that("Is invalid place of death, wrong string", {
  data <- data.frame(pod = "random place", other = 1)
  expect_equal(FALSE, is_valid_pod(data$pod))
})


##### Certification type #####

test_that("Is invalid certtype", {
  data <- data.frame(certtype = c(NA, 1.1, 14, 15, 1),
                     other = c(1:5))
  expect_equal(c(TRUE, FALSE, TRUE, FALSE, TRUE),
               is_valid_certtype(data$certtype))
})



##### Publication week and year validation #####

test_that("Week number and year for publication dod and dor, all conditions", {
  dummy_data <- data.frame(dor = c(20200601, 20230601, 20230601, NA),
                           reg_stat_dod = c(20210601, 20210601, NA, 20210601),
                           dor_pub_year = c(2020, 2023, 2023, NA),
                           reg_stat_dod_pub_year = c(2021, 2021, NA, 2021),
                           dor_pub_week  = c(23, 22, NA, 22),
                           reg_stat_dod_pub_week = c(22, 22, NA, 22))
  expected <- list(
    dor_week_na_count = 1,
    dor_year_na_count = 1,
    dod_week_na_count = 1,
    dod_year_na_count = 1,
    dor_before_dod = 1)

  expect_equal(suppressMessages(pub_week_year_validation(dummy_data)), expected)
})

test_that("Week number and year for publication dod and dor, no conditions", {
  dummy_data <- data.frame(dor = rep(20230601, 4),
                           reg_stat_dod = rep(20210601, 4),
                           dor_pub_year = c(2020, rep(2023, 3)),
                           reg_stat_dod_pub_year = rep(2021, 4),
                           dor_pub_week  = rep(22, 4),
                           reg_stat_dod_pub_week = rep(23, 4))
  expected <- list(
    dor_week_na_count = 0,
    dor_year_na_count = 0,
    dod_week_na_count = 0,
    dod_year_na_count = 0,
    dor_before_dod = 0)

  expect_equal(pub_week_year_validation(dummy_data), expected)
  expect_no_message(pub_week_year_validation(dummy_data))
})

test_that("Week number and year for pub dod and dor, message NA in dod year", {
  dummy_data <- data.frame(dor = rep(20230601, 2),
                           reg_stat_dod = c(NA, 20210601),
                           dor_pub_year = rep(2023, 2),
                           reg_stat_dod_pub_year = c(NA, 2021),
                           dor_pub_week  = rep(22, 2),
                           reg_stat_dod_pub_week = rep(22, 2))

  expect_message(pub_week_year_validation(dummy_data),
                 paste("Number of records that couldn't be assigned with a",
                       "year: Registrations: 0 Occurrences: 1"))
})

test_that("Week number and year for pub dod and dor, message NA in dor year", {
  dummy_data <- data.frame(dor = c(NA, 20230601),
                           reg_stat_dod = rep(20210601, 2),
                           dor_pub_year = c(NA, 2023),
                           reg_stat_dod_pub_year = rep(2021, 2),
                           dor_pub_week  = rep(22, 2),
                           reg_stat_dod_pub_week = rep(22, 2))

  expect_message(pub_week_year_validation(dummy_data),
                 paste("Number of records that couldn't be assigned with a",
                       "year: Registrations: 1 Occurrences: 0"))
})

test_that("Week number and year for publication dod and dor, NA in dor week", {
  dummy_data <- data.frame(dor = rep(20230601, 2),
                           reg_stat_dod = rep(20210601, 2),
                           dor_pub_year = rep(2023, 2),
                           reg_stat_dod_pub_year = rep(2021, 2),
                           dor_pub_week  = c(23, NA),
                           reg_stat_dod_pub_week = rep(22, 2))

  expect_message(pub_week_year_validation(dummy_data),
                 paste("Number of records that couldn't be assigned with a",
                       "week number: Registrations: 1 Occurrences: 0"))
})

test_that("Week number and year for publication dod and dor, NA in dod week", {
  dummy_data <- data.frame(dor = rep(20230601, 2),
                           reg_stat_dod = rep(20210601, 2),
                           dor_pub_year = rep(2023, 2),
                           reg_stat_dod_pub_year = rep(2021, 2),
                           dor_pub_week  = rep(23, 2),
                           reg_stat_dod_pub_week = c(23, NA))

  expect_message(pub_week_year_validation(dummy_data),
                 paste("Number of records that couldn't be assigned with a",
                       "week number: Registrations: 0 Occurrences: 1"))
})

test_that("Week number and year for publication dod and dor, NA in dod week", {
  dummy_data <- data.frame(dor = c(20210601, 20210601, 20230601),
                           reg_stat_dod = rep(20230601, 3),
                           dor_pub_year = rep(2023, 3),
                           reg_stat_dod_pub_year = rep(2021, 3),
                           dor_pub_week  = rep(23, 3),
                           reg_stat_dod_pub_week = rep(23, 3))

  expect_message(pub_week_year_validation(dummy_data),
                 paste("There are 2 records that have registration",
                       "before occurences. Check data."))
})

##### Interpolated population validation #####

test_that("Is valid year", {
  data <- data.frame(
    year = c(2020, 2100, 20.25, -2020, NA, 202),
    other = 1:6)
  expect_equal(c(TRUE, rep(FALSE, 5)), is_valid_year(data$year))

  data <- data.frame(
    year = c("2020", NA),
    other = 1:2)
  expect_error(is_valid_year(data$year))
})

test_that("Is valid dor week", {
  data <- data.frame(
    week = c(1:53, -1, 0.5, 54, NA),
    other = 1:57)
  expect_equal(c(rep(TRUE, 53), rep(FALSE, 4)), is_valid_dor_week(data$week))
})

test_that("Is valid UK country", {
  data <- data.frame(
    ctry = c("England, Wales and non-residents", "England", "Wales",
             "Northern Ireland", "Scotland", "england", "other", NA),
    other = 1:8)
  expect_equal(c(rep(TRUE, 5), rep(FALSE, 3)), is_valid_ctry_uk(data$ctry))
})

test_that("Is valid sex string", {
  data <- data.frame(
    sex = c("Male", "Female", "other", NA),
    other = 1:4)
  expect_equal(rep(c(TRUE, FALSE), each = 2), is_valid_sex_str(data$sex))
})

test_that("Is valid population", {
  data <- data.frame(
    pop = c(100, 1000.1, 123456789, -1000, -1000.1, NA),
    other = 1:6)
  expect_equal(rep(c(TRUE, FALSE), each = 3), is_valid_pop(data$pop))
})

test_that("Interpolated pops validation report generated correctly, no fails", {
  data <- data.frame(
    dor_year = rep(2023:2025, 2),
    dor_week = 1:6,
    esp_age_group = c("<1", "01-04", "15-19", "90+", "45-49", "60-64"),
    sex = rep(c("Male", "Female"), 3),
    population = c(100, 200, 300, 4000, 100.5, 91),
    area = c("England, Wales and non-residents", "England", "Wales",
             "Northern Ireland", rep("Scotland", 2)),
    other = 1:6)

  all_pass <- list(
    pass_count = 6,
    fail_count = 0,
    null_count = 0,
    fail_indices = numeric(),
    null_indices = numeric()
  )

  expected <- list(
    dor_year = all_pass,
    dor_week = all_pass,
    esp_age_group = all_pass,
    sex = all_pass,
    population = all_pass,
    area = all_pass
  )

  actual <- validate_weekly_pop(data)
  expect_equal(expected, actual)
})

test_that("Interpolated pops validation report generated correctly, one warn", {
  data <- data.frame(
    dor_year = rep(2023:2025, 2),
    dor_week = 1:6,
    esp_age_group = c("<1", "01-04", "15-19", "90+", "45-49", "100+"),
    sex = rep(c("Male", "Female"), 3),
    population = c(100, 200, 300, 4000, 100.5, 91),
    area = c("England, Wales and non-residents", "England", "Wales",
             "Northern Ireland", rep("Scotland", 2)),
    other = 1:6)

  all_pass <- list(
    pass_count = 6,
    fail_count = 0,
    null_count = 0,
    fail_indices = numeric(),
    null_indices = numeric()
  )

  expected <- list(
    dor_year = all_pass,
    dor_week = all_pass,
    esp_age_group = list(
      pass_count = 5,
      fail_count = 1,
      null_count = 0,
      fail_indices = 6,
      null_indices = numeric()),
    sex = all_pass,
    population = all_pass,
    area = all_pass
  )

  expect_equal(expected, suppressWarnings(validate_weekly_pop(data)))

  expect_warning(
    validate_weekly_pop(data),
    paste("There are 1 records that fail validation for is_valid_agegrp.",
          "Check validation report."))
})


test_that("Interpolated pops valid report generated, error due to NA", {
  data <- data.frame(
    dor_year = rep(2023:2025, 2),
    dor_week = 1:6,
    esp_age_group = c("<1", "01-04", "15-19", "90+", "45-49", NA),
    sex = rep(c("Male", "Female"), 3),
    population = c(100, 200, 300, 4000, 100.5, 91),
    area = c("England, Wales and non-residents", "England", "Wales",
             "Northern Ireland", rep("Scotland", 2)),
    other = 1:6)

  expect_error(
    validate_weekly_pop(data),
    "There are NA in the weekly population data. Check it.")
})
