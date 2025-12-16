
test_that("EW weekly deaths processed", {

  max_reg_date <- "20230106"

  data <- data.frame(
    dor_week = c(rep(1, 5), 2),
    reg_stat_dod = rep(1, 6),
    dor = c(rep(20230105, 5), 20250101),
    ageinyrs = rep(0, 6),
    agegroup_wide = rep("<1", 6),
    agegroup_2 = rep("<1", 6),
    pod = c("Other communal establishment", "Elsewhere", rep("Home", 2), NA,
            NA),
    sex = c(1, 2, 2, 1, 1, 1),
    ctrynm = c(rep(NA, 3), "England", "Wales", "Wales"),
    region = c(rep("London", 3), NA, rep("East of England", 2)),
    certtype = c(rep(1, 2), 3, 5, 12, 2),
    other_col = rep(1, 6),
    pcdr = c("EF 123", "EF123", rep("AB123C", 4))
  )

  dummy_nspl <- data.frame(
    pcd = c("EF 123", "AB123CD", "AB123C", "GH25PL"),
    gor = c("X1234", "Y1234", "Y1234", "X5678"),
    lsoa11 = c("X001", "Y001", "Y001", "X001")
  )

  expected <- data.frame(
    dod = rep(1, 5),
    agegrp_wide = rep("Under 1", 5),
    agegrp_5yr = rep("Under 1", 5),
    country = c(rep("Nonresident", 3), "England", "Wales"),
    place_of_death = c("Elsewhere", "Elsewhere", rep("Home", 2), NA),
    dor = c(rep(20230105, 5)),
    ageinyrs = rep(0, 5),
    sex = c("Male", "Female", "Female", "Male", "Male"),
    region = c(rep("London", 3), NA, "East of England"),
    certtype = c(rep(1, 2), 3, 5, 12),
    other_col = rep(1, 5),
    cert_type = c(rep("Certified by doctor", 2),
                  "Certified by coroner with inquest",
                  "Certified by coroner with no inquest",
                  "Certified by medical examiner"),
    cert_type_agg = c(rep("Certified by doctor", 2),
                      rep("Certified by coroner", 2),
                      "Certified by medical examiner"),
    lsoa11 = c("X001", "X001", rep("Y001", 3))
  )
  actual <- process_ew_weekly_deaths(data, dummy_nspl, max_reg_date)

  expect_equal(actual, expected)
})

test_that("EW weekly deaths created from annual data", {

  dummy_data <- data.frame(
    dor = c(rep(20230105, 2), 20250101),
    dod = rep(1, 3),
    agegroup_2 = c("45-49", "45-49", "90+"),
    ageinyrs = c(46, 46, 101),
    cestrss = c("H", "E", NA),
    esttyped = rep(NA, 3),
    nhsind = rep(1, 3),
    certtype = c(1, 5, 12),
    sex = c(rep(2, 2), 1),
    pcdr = c(rep("a a", 2), "bb"),
    ctryir = rep(1, 3),
    other = 1:3
  )

  dummy_nspl <- data.frame(
    pcd = c("aa", "bb"),
    gor = c("E12000001", "W99999999")
  )

  expected <- data.frame(
    dor = c(rep(20230105, 2), 20250101),
    dod = rep(1, 3),
    ageinyrs = c(46, 46, 101),
    certtype = c(1, 5, 12),
    sex = c(rep("Female", 2), "Male"),
    other = 1:3,
    cert_type = c("Certified by doctor",
                  "Certified by coroner with no inquest",
                  "Certified by medical examiner"),
    cert_type_agg = c("Certified by doctor", "Certified by coroner",
                      "Certified by medical examiner"),
    agegrp_5yr = c("45 to 49", "45 to 49", "100 and over"),
    agegrp_wide = c("45 to 64", "45 to 64", "85 and over"),
    place_of_death = c("Home", "Elsewhere", "Elsewhere"),
    region = c("North East", "North East", "Wales"),
    country = c("England", "England", "Wales")
  )
  actual <- process_ew_annual(dummy_data, dummy_nspl)

  expect_equal(actual, expected)
})

test_that("Scot weekly deaths processed", {

  data <- data.frame(
    dor = c(rep(20230105, 4), 20250101),
    dod = rep(20210101, 5),
    ageband_wide = rep("<1", 5),
    sex = c(1, 2, 2, 1, 1),
    other_col = rep(1, 5)
  )

  test_cfg <- list(week = 1, year = 2023, scope = "UK")

  expected <- data.frame(
    dor = c(rep(20230105, 4)),
    dod = rep(20210101, 4),
    agegrp_wide = rep("Under 1", 4),
    sex = c("Male", "Female", "Female", "Male"),
    other_col = rep(1, 4),
    country = rep("Scotland", 4)
  )

  actual <- process_scot_weekly_deaths(data, test_cfg)

  expect_equal(actual, expected)
})

test_that("Scot weekly deaths processed with EW scope", {

  data <- data.frame(
    dor = c(rep(20230105, 4), 20230112, 20250101),
    dod = rep(20210101, 6),
    ageband_wide = rep("<1", 6),
    sex = c(1, 2, 2, 1, 1, 1),
    other_col = rep(1, 6)
  )

  test_cfg <- list(week = 2, year = 2023, scope = "EW")

  expected <- data.frame(
    dor = c(rep(20230105, 4)),
    dod = rep(20210101, 4),
    agegrp_wide = rep("Under 1", 4),
    sex = c("Male", "Female", "Female", "Male"),
    other_col = rep(1, 4),
    country = rep("Scotland", 4)
  )

  actual <- process_scot_weekly_deaths(data, test_cfg)

  expect_equal(actual, expected)
})

test_that("NI weekly deaths processed", {

  max_reg_date <- "20230106"

  data <- data.frame(
    dor = c(rep(20230105, 4), 20250101),
    dod = rep(20210101, 5),
    age_wide = rep("<1", 5),
    sex = c(1, 2, 2, 1, 1),
    other_col = rep(1, 5)
  )

  expected <- data.frame(
    dor = c(rep(20230105, 4)),
    dod = rep(20210101, 4),
    agegrp_wide = rep("Under 1", 4),
    sex = c("Male", "Female", "Female", "Male"),
    other_col = rep(1, 4),
    country = rep("Northern Ireland", 4)
  )

  expect_equal(expected, process_ni_weekly_deaths(data, max_reg_date, "UK"))
})

test_that("NI weekly deaths processed with EW scope", {

  max_reg_date <- "20230113"

  data <- data.frame(
    dor = c(rep(20230105, 4), 20230112, 20250101),
    dod = rep(20210101, 6),
    age_wide = rep("<1", 6),
    sex = c(1, 2, 2, 1, 1, 1),
    other_col = rep(1, 6)
  )

  expected <- data.frame(
    dor = c(rep(20230105, 4)),
    dod = rep(20210101, 4),
    agegrp_wide = rep("Under 1", 4),
    sex = c("Male", "Female", "Female", "Male"),
    other_col = rep(1, 4),
    country = rep("Northern Ireland", 4)
  )

  expect_equal(expected, process_ni_weekly_deaths(data, max_reg_date, "EW"))
})

test_that("Age group processed", {

  data <- data.frame(
    a = 1:6,
    b = c("75+", "20-24", "<1", "20-24", "05-09", NA),
    c = c("85+", "01-04", "<1", "20-24", "05-09", NA)
  )

  expected <- data.frame(
    a = 1:6,
    b = c("75+", "20-24", "<1", "20-24", "05-09", NA),
    c = c("85 and over", "1 to 4", "Under 1", "20 to 24", "5 to 9", NA)
  )

  expect_equal(expected, process_agegrp(data, c))
})

test_that("Change max age group to 100 and over", {

  data <- data.frame(
    a = 1:8,
    ageinyrs = c(10, 89, 90, 95, 99, 100, 105, NA),
    agegroup_2 = c("10 to 14", "85 to 89", rep("90 and over", 5), NA)
  )

  expected <- data.frame(
    a = 1:8,
    ageinyrs = c(10, 89, 90, 95, 99, 100, 105, NA),
    agegroup_2 = c("10 to 14", "85 to 89", "90 to 94",
                   rep(c("95 to 99", "100 and over"), each = 2), NA)
  )

  expect_equal(expected, change_max_agegrp(data))
})

test_that("Change max age group to 90 and over", {

  data <- data.frame(
    a = 1:8,
    agegrp_5yr = c("10 to 14", "85 to 89", "90 to 94",
                   rep(c("95 to 99", "100 and over"), each = 2), NA)
  )

  expected <- data.frame(
    a = 1:8,
    agegroup_2 = c("10 to 14", "85 to 89", rep("90 and over", 5), NA)
  )

  expect_equal(expected, reverse_max_agegrp(data))
})

test_that("Error for flag_cause with wrong due_to argument", {

  data <- data.frame(a = 1:3)

  expect_error(flag_cause(data, col_1, "x", due_to = "z"),
               "due_to argument must be TRUE or FALSE")
})

test_that("Creating cause flags, default due_to (FALSE), involving the code", {

  data <- data.frame(
    fic10und = c("A100", "B18", "C999"),
    fic1 = c("B201", "D25", "A00"),
    fic_2 = c(NA, "B500", "B600"),
    xfic = c(NA, "A09", NA),
    other = c(NA, "A09", NA)
  )

  expected <- data.frame(
    fic10und = c("A100", "B18", "C999"),
    fic1 = c("B201", "D25", "A00"),
    fic_2 = c(NA, "B500", "B600"),
    xfic = c(NA, "A09", NA),
    other = c(NA, "A09", NA),
    cause_a = c(TRUE, FALSE, TRUE)
  )

  actual <- flag_cause(data, cause_a, c("A00", "A09", "A10", "A99"))
  expect_equal(actual, expected)
})

test_that("Creating cause flags, due_to = TRUE", {

  data <- data.frame(
    # When the required code is 4 digit,
    # it doesn't match the 3-digit code in the data
    fic10und = c("A100", "B180", "B18"),
    fic1 = c("B20", "D25", "A002"),
    fic_2 = c(NA, "B500", "B600"),
    xfic = c(NA, "A09", NA),
    other = c(NA, "A09", NA)
  )

  expected <- data.frame(
    fic10und = c("A100", "B180", "B18"),
    fic1 = c("B20", "D25", "A002"),
    fic_2 = c(NA, "B500", "B600"),
    xfic = c(NA, "A09", NA),
    other = c(NA, "A09", NA),
    cause_a = c(FALSE, TRUE, FALSE)
  )

  actual <- flag_cause(data, cause_a, c("B180", "B20", "B60"), TRUE)
  expect_equal(actual, expected)
})

test_that("Output of flag_agegrp_wide is correct", {

  data <- data.frame(
    age = c(0:86, NA),
    other = 1:88
  )

  expected <- data.frame(
    age = c(0:86, NA),
    other = 1:88,
    agegrp_wide = c(
      "Under 1", rep("1 to 14", 14), rep("15 to 44", 30), rep("45 to 64", 20),
      rep("65 to 74", 10), rep("75 to 84", 10), rep("85 and over", 2), NA))

  actual <- flag_agegrp_wide(data, age)

  expect_equal(actual, expected)
})

test_that("output of flag_ew_country", {

  data <- data.frame(
    col_1 = 1:5,
    ctryr = c(921, 924, 942, 912, 1)
  )

  expected <- data.frame(
    col_1 = 1:5,
    ctryr = c(921, 924, 942, 912, 1),
    country = c("England", "Wales", rep("Nonresident", 3))
  )

  actual <- flag_country_ew(data)

  expect_equal(actual, expected)
})

test_that("Creating other place of death flags, data_type is provisonal", {

  data <- data.frame(
    place_of_death = c("Home", "Hospice", "Elsewhere", NA, "Hospital"),
    other = 1:5
  )

  expected <- data.frame(
    other = 1:5,
    place_of_death_other = c("Home", rep("Other", 2), NA, "Hospital")
  )

  expect_equal(flag_place_of_death_other(data, "provisional"), expected)
})

test_that("Creating other place of death flags, data_type is finalised", {

  data <- data.frame(
    cestrss = c("H", "E", "11100", "22200", "33000", "44000"),
    esttyped = c(NA, NA, 7, 7, 18, 99),
    other = 1:6,
    nhsind = c(NA, NA, 2, 1, 2, 1)
  )

  expected <- data.frame(
    other = 1:6,
    place_of_death_other = c(
      "Home", "Other", "Care home", "Care home", "Hospital", "Hospital"))

  expect_equal(flag_place_of_death_other(data, "finalised"), expected)
})

test_that("Error for other place of death flag with wrong data_type argument", {

  data <- data.frame(a = 1:3)

  expect_error(flag_place_of_death_other(data, "other string"),
               'data_type argument must be "finalised" or "provisional"')
})

test_that("Output of process_prev_years", {

  data <- data.frame(
    dor = c(20181201, 20191102, 20210101, 20220504, 20230303, 20200505),
    ctryr = c(921, 921, 924, 900, 924, 921),
    sex = c(1, 2, 1, 2, 2, 1),
    cestrss = c("H", "E", "11100", "22200", "33000", "33000"),
    esttyped = c(NA, NA, 7, 7, 18, 18),
    fic10und = c("J00", "C50", "A100", "B20", "B20", "J10"),
    fic10men1 = c(NA, "J10", "C20", "D50", NA, NA),
    other = c(1:6),
    ageinyrs = c(45, 67, 18, 80, 90, 22),
    nhsind = c(NA, NA, 2, 1, 2, 1)
  )

  years_final <- c(2018, 2019, 2021, 2022)

  expected <- data.frame(
    dor = c(20181201, 20191102, 20220504),
    ctryr = c(921, 921, 900),
    sex = c("Male", "Female", "Female"),
    fic10und = c("J00", "C50", "B20"),
    fic10men1 = c(NA, "J10", "D50"),
    other = c(1, 2, 4),
    ageinyrs = c(45, 67, 80),
    year = c(2018, 2019, 2022),
    week = c(49, 45, 18),
    week_ending = c("7 December 2018", "8 November 2019", "6 May 2022"),
    agegrp_wide = c("45 to 64", "65 to 74", "75 to 84"),
    country = c("England", "England", "Nonresident"),
    place_of_death_other = c("Home", "Other", "Care home")
  )

  expect_equal(process_prev_years(data, years_final), expected)
})

test_that("Place of deaths flags created for annual data", {

  dummy_data <- data.frame(
    cestrss = c("H", "E", rep("11", 24)),
    esttyped = c(NA, NA, 1, 3, 18, 99, 1, 18, 19, 2, 4, 7, 10, 21, 3, 4, 7, 10,
                 14, 20, 22, 32, 33, 83, 83, 99),
    nhsind = c(NA, NA, rep(1, 4), rep(2, 3), rep(1, 5), rep(2, 9), 1, 2, 2),
    other = 1:26
  )

  expected <- data.frame(
    other = 1:26,
    place_of_death = c("Home", "Elsewhere", rep("Hospital", 7),
                       rep("Care home", 14), rep("Hospice", 2), "Elsewhere")
  )

  actual <- flag_place_of_death_annual(dummy_data)
  expect_equal(actual, expected)
})

test_that("Gor codes converted into regions and countries", {

  dummy_data <- data.frame(
    pcdr = c("a", "b", "j", "c", "d", "e", "f", "g", "h", "i", "j"),
    other = 1:11
  )

  dummy_nspl <- data.frame(
    pcd = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"),
    gor = c(paste0("E1200000", 1:9), "W99999999")
  )

  expected <- data.frame(
    other = 1:11,
    region = c("North East", "North West", "Wales", "Yorkshire and The Humber",
               "East Midlands", "West Midlands", "East of England", "London",
               "South East", "South West", "Wales"),
    country = c("England", "England", "Wales", rep("England", 7), "Wales")
  )

  actual <- flag_country_region(dummy_data, dummy_nspl)

  expect_equal(actual, expected)
})


test_that("Derive certification types", {

  dummy_data <- data.frame(
    certtype = c(NA, 1:14, 1.1),
    other = 1:16
  )

  expected <- data.frame(
    certtype = c(NA, 1:14, 1.1),
    other = 1:16,
    cert_type = c(
      "Unknown/Uncertified", rep("Certified by doctor", 2),
      rep("Certified by coroner with inquest", 2),
      "Certified by coroner with no inquest", "Unknown/Uncertified",
      "Certified by doctor", "Certified by coroner with inquest",
      "Unknown/Uncertified", rep("Certified by medical examiner", 3),
      rep("Certified by coroner with no inquest", 2), NA_character_)
  )

  actual <- flag_cert_type_groups(dummy_data, certtype)

  expect_equal(actual, expected)
})


test_that("Derive certification type by aggregation", {

  dummy_data <- data.frame(
    other = 1:16,
    cert_type = c(
      "Unknown/Uncertified", rep("Certified by doctor", 2),
      rep("Certified by coroner with inquest", 2),
      "Certified by coroner with no inquest", "Unknown/Uncertified",
      "Certified by doctor", "Certified by coroner with inquest",
      "Unknown/Uncertified", rep("Certified by medical examiner", 3),
      "Certified by coroner with no inquest", rep(NA_character_, 2))
  )

  expected <- data.frame(
    other = 1:16,
    cert_type = c(
      "Unknown/Uncertified", rep("Certified by doctor", 2),
      rep("Certified by coroner with inquest", 2),
      "Certified by coroner with no inquest", "Unknown/Uncertified",
      "Certified by doctor", "Certified by coroner with inquest",
      "Unknown/Uncertified", rep("Certified by medical examiner", 3),
      "Certified by coroner with no inquest",
      rep(NA_character_, 2)),
    cert_type_agg = c(
      "Unknown/Uncertified", rep("Certified by doctor", 2),
      rep("Certified by coroner", 3),
      "Unknown/Uncertified",
      "Certified by doctor", "Certified by coroner",
      "Unknown/Uncertified", rep("Certified by medical examiner", 3),
      "Certified by coroner", rep(NA_character_, 2))
  )

  actual <- flag_cert_type_agg(dummy_data, cert_type)

  expect_equal(actual, expected)
})
