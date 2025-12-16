test_that("Create UK dataframe from EW/Scot/NI (inc dor < year start)", {

  lvls <- create_factor_levels()

  ew_df <- data.frame(
    dor = c(20250101:20250121, 20240101),
    agegrp_5yr = lvls$agegrp_5yr[-1],
    country = rep(c("England", "Wales"), each = 11),
    other = 1:22
  )

  scot_df <- data.frame(
    agegroup_2 = c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                   "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                   "60-64", "65-69", "70-74", "75-79", "80-84", "85-89",
                   rep("90+", 3)),
    covid = c(rep(0, 20), 1, 0),
    dor = c(20250101:20250120, 20250101, 20240101),
    country = rep("Scotland", 22),
    deaths = seq(10, 220, 10),
    other = 1:22
  )

  ni_df <- data.frame(
    agegroup_2 = c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                   "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                   "60-64", "65-69", "70-74", "75-79", "80-84", "85-89",
                   rep("90+", 3)),
    dor = c(20250101:20250120, 20240101, 20240101),
    country = rep("Northern Ireland", 22),
    deaths = seq(10, 220, 10),
    other = 1:22
  )

  expected <- data.frame(
    dor = c(20250101:20250121, 20240101, rep(20250101:20250120, 2)),
    country = c(rep(c("England", "Wales"), each = 11),
                rep(c("Scotland", "Northern Ireland"), each = 20)),
    other = c(1:22, rep(1:20, 2)),
    agegroup_2 = c(lvls$agegrp_5yr[2:20], rep("90 and over", 3),
                   rep(c(lvls$agegrp_5yr[2:20], "90 and over"), 2)),
    deaths = c(rep(1, 22), rep(seq(10, 200, 10), 2)),
    covid = c(rep(NA, 22), rep(0, 20), rep(NA, 20))
  )

  output <- create_uk_df(ew_df, scot_df, ni_df, 2025)

  expect_equal(output, expected, ignore_attr = TRUE)
})


test_that("Create UK dataframe from EW/Scot/NI, some NA", {

  lvls <- create_factor_levels()

  ew_df <- data.frame(
    dor = c(20250101:20250120, NA, 20240101),
    agegrp_5yr = lvls$agegrp_5yr[-1],
    country = c(rep(c("England", "Wales"), each = 10), NA, NA),
    other = 1:22
  )

  scot_df <- data.frame(
    agegroup_2 = c(NA, "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                   "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                   "60-64", "65-69", "70-74", "75-79", "80-84", "85-89",
                   rep("90+", 3)),
    covid = c(0, NA, rep(0, 18), 1, 0),
    dor = c(20250101:20250120, 20250101, 20240101),
    country = rep("Scotland", 22),
    deaths = c(10, 20, NA, seq(40, 220, 10)),
    other = 1:22
  )

  ni_df <- data.frame(
    agegroup_2 = c(NA, "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                   "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                   "60-64", "65-69", "70-74", "75-79", "80-84", "85-89",
                   rep("90+", 3)),
    dor = c(20250101:20250120, 20240101, 20240101),
    country = rep("Northern Ireland", 22),
    deaths = c(10, 20, NA, seq(40, 220, 10)),
    other = 1:22
  )

  expected <- data.frame(
    dor = c(20250101:20250120, NA, 20240101, 20250101, 20250103:20250120,
            20250101:20250120),
    country = c(rep(c("England", "Wales"), each = 10), NA, NA,
                rep("Scotland", 19), rep("Northern Ireland", 20)),
    other = c(1:22, 1, 3:20, 1:20),
    agegroup_2 = c(lvls$agegrp_5yr[2:20], rep("90 and over", 3),
                   c(NA, lvls$agegrp_5yr[4:20], "90 and over"),
                   c(NA, lvls$agegrp_5yr[3:20], "90 and over")),
    deaths = c(rep(1, 22), c(10, NA, seq(40, 200, 10)),
               c(10, 20, NA, seq(40, 200, 10))),
    covid = c(rep(NA, 22), rep(0, 19), rep(NA, 20))
  )

  output <- create_uk_df(ew_df, scot_df, ni_df, 2025)

  expect_equal(output, expected, ignore_attr = TRUE)
})


test_that("Count UK dths by ctry, age, sex by week, default age group", {

  # Long dummy data to integrate with test-utils.R of subfunction
  data <- data.frame(
    year = rep(2025, 119),
    week = c(rep(1, 104), rep(2, 15)),
    week_ending = c(rep("31 October 2021", 104), rep("14 February 2021", 15)),
    country =
      c("England", "Wales", rep("Nonresident", 3),
        rep("Northern Ireland", 4), rep("Scotland", 5), rep("England", 6),
        rep("Wales", 7), rep("Nonresident", 8), rep("Northern Ireland", 9),
        rep("Scotland", 10), rep("England", 11), rep("Wales", 12),
        rep("Nonresident", 13), rep("Northern Ireland", 14),
        rep("Scotland", 15)),
    sex =
      c("Male", "Female", rep("Male", 3), rep("Female", 4),
        rep("Male", 5), rep("Female", 6), rep("Male", 7), rep("Female", 8),
        rep("Male", 9), rep("Female", 10), rep("Male", 11), rep("Female", 12),
        rep("Male", 13), rep("Female", 14), rep("Male", 15)),
    agegrp_wide =
      c(rep("Under 1", 2), rep("1 to 84", 3), rep("Over 85", 4),
        rep("Under 1", 5), rep("1 to 84", 6), rep("Over 85", 7),
        rep("Under 1", 8), rep("1 to 84", 9), rep("Over 85", 10),
        rep("Under 1", 11), rep("1 to 84", 12), rep("Over 85", 13),
        rep("Under 1", 14), rep("Under 1", 15)),
    agegroup_2 =
      c(rep("Under 1", 2), rep("80 to 84", 3), rep("85 to 89", 4),
        rep("Under 1", 5), rep("80 to 84", 6), rep("85 to 89", 7),
        rep("Under 1", 8), rep("80 to 84", 9), rep("85 to 89", 10),
        rep("Under 1", 11), rep("80 to 84", 12), rep("85 to 89", 13),
        rep("Under 1", 14), rep("Under 1", 15)),
    deaths = c(1, 2, rep(1, 117))
  )

  expected <- data.frame(
    year = rep(2025, 63),
    week =
      c(rep(1, 10), 2, rep(1, 11), 2, rep(1, 12), rep(2, 2), rep(1, 16),
        rep(2, 2), rep(1, 6), rep(2, 2)),
    country =
      c(rep("England", 2), rep("Northern Ireland", 3),
        rep("Scotland", 2),
        rep("Wales", 3), "Scotland",
        rep("England, Wales and non-residents", 5),
        rep("UK", 7),
        rep(c("England", "England, Wales and non-residents",
              "Northern Ireland", "Scotland", "UK", "Wales"), 2),
        "Scotland", "UK",
        rep("England", 2), rep(c("England, Wales and non-residents",
                                 "Northern Ireland"), each = 3),
        rep("Scotland", 2), rep(c("UK", "Wales"), each = 3),
        "Scotland", "UK", "England", "England, Wales and non-residents",
        "Northern Ireland", "Scotland", "UK", "Wales", "Scotland",
        "UK"),
    agegrp_wide =
      c("1 to 84", "Under 1", "1 to 84", "Over 85", "Under 1",
        "Over 85", "Under 1", "1 to 84",
        "Over 85", rep("Under 1", 2), "1 to 84",
        "Under 1", "1 to 84", "Over 85", "Under 1",
        rep(c("1 to 84", "Over 85", "Under 1"), 2), "Under 1",
        rep("All ages", 14),
        "1 to 84", "Under 1",
        rep(c("1 to 84", "Over 85", "Under 1"), 2), "Over 85",
        "Under 1", rep(c("1 to 84", "Over 85", "Under 1"), 2),
        rep("Under 1", 2), rep("All ages", 8)),
    sex =
      c("Female", rep(c("Male", "Female"), each = 2), "Female",
        rep(c("Male",
              "Female"), 2), "Male", rep("Female", 2), rep("Male", 3),
        rep("Female", 3), rep("Male", 4),
        rep("Female", 6), rep("Male", 8),
        rep("All people", 26)),
    deaths =
      c(6, 12, 9, 4, 14, 10, 5, 12, 7, 2, 15, 18, 10, 3, 20,
        12, 18, 14, 24, 12, 20, 17, 15, 6, 28, 18, 10, 56, 14, 12, 35, 9,
        5, 49, 7, 15, 15, 6, 12, 21, 20, 22, 9, 4, 14, 10, 5, 30, 34, 41,
        12, 7, 2, 15, 15, 18, 63, 27, 15, 105, 21, 15, 15)
  )

  output <- count_uk_dths_ctry_age_sex(data)

  expect_equal(output, expected, ignore_attr = TRUE)
})


test_that("Count UK dths by ctry, age, sex by week for agegroup_2", {

  data <- data.frame(
    year = rep(2025, 119),
    week = c(rep(1, 104), rep(2, 15)),
    week_ending = c(rep("31 October 2021", 104), rep("14 February 2021", 15)),
    country =
      c("England", "Wales", rep("Nonresident", 3),
        rep("Northern Ireland", 4), rep("Scotland", 5), rep("England", 6),
        rep("Wales", 7), rep("Nonresident", 8), rep("Northern Ireland", 9),
        rep("Scotland", 10), rep("England", 11), rep("Wales", 12),
        rep("Nonresident", 13), rep("Northern Ireland", 14),
        rep("Scotland", 15)),
    sex =
      c("Male", "Female", rep("Male", 3), rep("Female", 4),
        rep("Male", 5), rep("Female", 6), rep("Male", 7), rep("Female", 8),
        rep("Male", 9), rep("Female", 10), rep("Male", 11), rep("Female", 12),
        rep("Male", 13), rep("Female", 14), rep("Male", 15)),
    agegrp_wide =
      c(rep("Under 1", 2), rep("1 to 84", 3), rep("Over 85", 4),
        rep("Under 1", 5), rep("1 to 84", 6), rep("Over 85", 7),
        rep("Under 1", 8), rep("1 to 84", 9), rep("Over 85", 10),
        rep("Under 1", 11), rep("1 to 84", 12), rep("Over 85", 13),
        rep("Under 1", 14), rep("Under 1", 15)),
    agegroup_2 =
      c(rep("Under 1", 2), rep("80 to 84", 3), rep("85 to 89", 4),
        rep("Under 1", 5), rep("80 to 84", 6), rep("85 to 89", 7),
        rep("Under 1", 8), rep("80 to 84", 9), rep("85 to 89", 10),
        rep("Under 1", 11), rep("80 to 84", 12), rep("85 to 89", 13),
        rep("Under 1", 14), rep("Under 1", 15)),
    deaths = c(1, 2, rep(1, 117))
  )

  expected <- data.frame(
    year = rep(2025, 63),
    week =
      c(rep(1, 10), 2, rep(1, 11), 2, rep(1, 12), rep(2, 2), rep(1, 16),
        rep(2, 2), rep(1, 6), rep(2, 2)),
    country =
      c(rep("England", 2), rep("Northern Ireland", 3),
        rep("Scotland", 2),
        rep("Wales", 3), "Scotland",
        rep("England, Wales and non-residents", 5),
        rep("UK", 7),
        rep(c("England", "England, Wales and non-residents",
              "Northern Ireland", "Scotland", "UK", "Wales"), 2),
        "Scotland", "UK",
        rep("England", 2), rep(c("England, Wales and non-residents",
                                 "Northern Ireland"), each = 3),
        rep("Scotland", 2), rep(c("UK", "Wales"), each = 3),
        "Scotland", "UK", "England", "England, Wales and non-residents",
        "Northern Ireland", "Scotland", "UK", "Wales", "Scotland",
        "UK"),
    agegroup_2 =
      c("80 to 84", "Under 1", "80 to 84", "85 to 89", "Under 1",
        "85 to 89", "Under 1", "80 to 84",
        "85 to 89", rep("Under 1", 2), "80 to 84",
        "Under 1", "80 to 84", "85 to 89", "Under 1",
        rep(c("80 to 84", "85 to 89", "Under 1"), 2), "Under 1",
        rep("All ages", 14),
        "80 to 84", "Under 1",
        rep(c("80 to 84", "85 to 89", "Under 1"), 2), "85 to 89",
        "Under 1", rep(c("80 to 84", "85 to 89", "Under 1"), 2),
        rep("Under 1", 2), rep("All ages", 8)),
    sex =
      c("Female", rep(c("Male", "Female"), each = 2), "Female",
        rep(c("Male",
              "Female"), 2), "Male", rep("Female", 2), rep("Male", 3),
        rep("Female", 3), rep("Male", 4),
        rep("Female", 6), rep("Male", 8),
        rep("All people", 26)),
    deaths =
      c(6, 12, 9, 4, 14, 10, 5, 12, 7, 2, 15, 18, 10, 3, 20,
        12, 18, 14, 24, 12, 20, 17, 15, 6, 28, 18, 10, 56, 14, 12, 35, 9,
        5, 49, 7, 15, 15, 6, 12, 21, 20, 22, 9, 4, 14, 10, 5, 30, 34, 41,
        12, 7, 2, 15, 15, 18, 63, 27, 15, 105, 21, 15, 15)
  )

  output <- count_uk_dths_ctry_age_sex(data, "agegroup_2")

  expect_equal(output, expected, ignore_attr = TRUE)
})


test_that("Group and find total counts of chosen column - All people, deaths", {

  data <- data.frame(
    year = rep(2025, 15),
    week = c(rep(1, 14), 2),
    week_ending = c(rep("31 October 2023", 14), "14 February 2021"),
    country = rep(c("Eng", "Wales", "Nonresident", "NI", "Scot"), 3),
    sex = c(rep(c("Male", "Female"), 7), "Male"),
    agegrp_wide = c("Under 1", rep(c("Under 1", "1 to 84", "Over 85"), 4),
                    "Under 1", "Under 1"),
    deaths = 1:15
  )

  expected <- data.frame(
    year = rep(2025, 14),
    week = c(rep(1, 13), 2),
    country = c(rep("Eng", 2), rep("NI", 3), rep("Nonresident", 3),
                rep("Scot", 2), rep("Wales", 3), "Scot"),
    agegrp_wide = c("1 to 84", "Under 1", "1 to 84", "Over 85", "Under 1",
                    "1 to 84", "Over 85", "Under 1", "Over 85", "Under 1",
                    "1 to 84", "Over 85", "Under 1", "Under 1"),
    sex = rep("All people", 14),
    deaths = c(6, 12, 9, 4, 14, 3, 13, 8, 10, 5, 12, 7, 2, 15)
  )

  output <- total_by_group(data,
                           grouping_cols = c("country", "agegrp_wide"),
                           total_col = "sex",
                           total_name = "All people")

  expect_equal(output, expected, ignore_attr = TRUE)
})


test_that("Group by country and agegroup_2, get total for All people, deaths", {

  data <- data.frame(
    year = rep(2025, 15),
    week = c(rep(1, 14), 2),
    week_ending = c(rep("31 October 2023", 14), "14 February 2021"),
    country = rep(c("Eng", "Wales", "Nonresident", "NI", "Scot"), 3),
    sex = c(rep(c("Male", "Female"), 7), "Male"),
    agegroup_2 = c("Under 1", rep(c("Under 1", "80 to 84", "85 to 89"), 4),
                    "Under 1", "Under 1"),
    deaths = 1:15
  )

  expected <- data.frame(
    year = rep(2025, 14),
    week = c(rep(1, 13), 2),
    country = c(rep("Eng", 2), rep("NI", 3), rep("Nonresident", 3),
                rep("Scot", 2), rep("Wales", 3), "Scot"),
    agegroup_2 = c("80 to 84", "Under 1", "80 to 84", "85 to 89", "Under 1",
                    "80 to 84", "85 to 89", "Under 1", "85 to 89", "Under 1",
                    "80 to 84", "85 to 89", "Under 1", "Under 1"),
    sex = rep("All people", 14),
    deaths = c(6, 12, 9, 4, 14, 3, 13, 8, 10, 5, 12, 7, 2, 15)
  )

  output <- total_by_group(data,
                           grouping_cols = c("country", "agegroup_2"),
                           total_col = "sex",
                           total_name = "All people")

  expect_equal(output, expected, ignore_attr = TRUE)
})


test_that("Group and find total counts of a chosen column - All ages, deaths", {

  data <- data.frame(
    year = rep(2025, 15),
    week = c(rep(1, 14), 2),
    week_ending = c(rep("31 October 2023", 14), "14 February 2021"),
    country = rep(c("Eng", "Wales", "Nonresident", "NI", "Scot"), 3),
    sex = c(rep(c("Male", "Female"), 7), "Male"),
    agegrp_wide = c("Under 1", rep(c("Under 1", "1 to 84", "Over 85"), 4),
                    "Under 1", "Under 1"),
    deaths = 1:15
  )

  expected <- data.frame(
    year = rep(2025, 11),
    week = c(rep(1, 10), 2),
    sex = c(rep(c("Female", "Male"), each = 5), "Male"),
    country = c(rep(c("Eng", "NI", "Nonresident", "Scot", "Wales"), 2),
                "Scot"),
    agegrp_wide = rep("All ages", 11),
    deaths = c(6, 18, 8, 10, 14, 12, 9, 16, 5, 7, 15)
  )

  output <- total_by_group(data,
                           grouping_cols = c("sex", "country"),
                           total_col = "agegrp_wide",
                           total_name = "All ages")

  expect_equal(output, expected, ignore_attr = TRUE)
})


test_that("Group and find total counts of a chosen column - UK, deaths", {

  data <- data.frame(
    year = rep(2025, 15),
    week = c(rep(1, 14), 2),
    week_ending = c(rep("31 October 2023", 14), "14 February 2021"),
    country = rep(c("Eng", "Wales", "Nonresident", "NI", "Scot"), 3),
    sex = c(rep(c("Male", "Female"), 7), "Male"),
    agegrp_wide = c("Under 1", rep(c("Under 1", "1 to 84", "Over 85"), 4),
                    "Under 1", "Under 1"),
    deaths = 1:15
  )

  expected <- data.frame(
    year = rep(2025, 7),
    week = c(rep(1, 6), 2),
    sex = c(rep(c("Female", "Male"), each = 3), "Male"),
    agegrp_wide = c(rep(c("1 to 84", "Over 85", "Under 1"), 2), "Under 1"),
    country = rep("UK", 7),
    deaths = c(18, 14, 24, 12, 20, 17, 15)
  )

  output <- total_by_group(data,
                           grouping_cols = c("sex", "agegrp_wide"),
                           total_col = "country",
                           total_name = "UK")

  expect_equal(output, expected, ignore_attr = TRUE)
})


test_that("Group and find total counts of chosen col - E+W+non-res, deaths", {

  # Date filtered before the function is run
  data <- data.frame(
    year = rep(2025, 9),
    week = c(rep(1, 8), 2),
    week_ending = c(rep("31 October 2023", 8), "14 February 2021"),
    country = rep(c("Eng", "Wales", "Nonresident"), 3),
    sex = c(rep(c("Male", "Female"), 4), "Male"),
    agegrp_wide = c("Under 1", rep(c("Under 1", "1 to 84", "Over 85"), 2),
                    "Under 1", "Under 1"),
    deaths = 1:9
  )

  expected <- data.frame(
    year = rep(2025, 7),
    week = c(rep(1, 6), 2),
    sex = c(rep(c("Female", "Male"), each = 3), "Male"),
    agegrp_wide = c(rep(c("1 to 84", "Over 85", "Under 1"), 2), "Under 1"),
    country = rep("England, Wales and Nonresidents", 7),
    deaths = c(6, 4, 10, 3, 7, 6, 9)
  )

  output <- total_by_group(data,
                           grouping_cols = c("sex", "agegrp_wide"),
                           total_col = "country",
                           total_name = "England, Wales and Nonresidents")

  expect_equal(output, expected, ignore_attr = TRUE)
})

test_that("Group and find total counts of chosen column - All people, pops", {

  data <- data.frame(
    year = rep(2025, 15),
    week = c(rep(1, 14), 2),
    week_ending = c(rep("31 October 2023", 14), "14 February 2021"),
    country = rep(c("Eng", "Wales", "Nonresident", "NI", "Scot"), 3),
    sex = c(rep(c("Male", "Female"), 7), "Male"),
    agegrp_wide = c("Under 1", rep(c("Under 1", "1 to 84", "Over 85"), 4),
                    "Under 1", "Under 1"),
    population = 1:15
  )

  expected <- data.frame(
    year = rep(2025, 14),
    week = c(rep(1, 13), 2),
    country = c(rep("Eng", 2), rep("NI", 3), rep("Nonresident", 3),
                rep("Scot", 2), rep("Wales", 3), "Scot"),
    agegrp_wide = c("1 to 84", "Under 1", "1 to 84", "Over 85", "Under 1",
                    "1 to 84", "Over 85", "Under 1", "Over 85", "Under 1",
                    "1 to 84", "Over 85", "Under 1", "Under 1"),
    sex = rep("All people", 14),
    population = c(6, 12, 9, 4, 14, 3, 13, 8, 10, 5, 12, 7, 2, 15)
  )

  output <- total_by_group(data,
                           grouping_cols = c("country", "agegrp_wide"),
                           total_col = "sex",
                           total_name = "All people",
                           sum_colname = "population")

  expect_equal(output, expected, ignore_attr = TRUE)
})


test_that("Format count UK dths by ctry, age, sex by week", {

  data <- data.frame(
    year = rep(2025, 24),
    week = rep(c(1, 2, 1, 1), 6),
    country = c("UK", "England, Wales and non-residents", "England", "Wales",
                "Scotland", "Northern Ireland"),
    agegrp_wide = rep(c("All ages", "Under 1", "1 to 14", "15 to 44",
                        "45 to 64", "65 to 74", "75 to 84", "85 and over"), 3),
    sex = rep(c("All people", "Male", "Female"), 8),
    deaths = 1:24
  )

  lvls <- create_factor_levels()

  expected <- data.frame(
    `Week number` = c(rep(2, 6), rep(1, 18)),
    Country = factor(
      c(rep(c("Wales", "Northern Ireland", "England, Wales and non-residents"),
            2), rep(c("UK", "England", "Scotland"), 2), "Wales",
        "Northern Ireland", "England, Wales and non-residents",
        rep(c("UK", "England", "Scotland"), 2), "Wales", "Northern Ireland",
        "England, Wales and non-residents"),
      levels = lvls$country),
    `Age group (years)` = factor(
      rep(c("Under 1", "65 to 74", "All ages", "1 to 14", "15 to 44",
            "45 to 64", "75 to 84", "85 and over"), each = 3),
      levels = lvls$agegrp_wide),
    Sex = factor(
      c(rep(c("All people", "Female", "Male"), 8)),
      levels = lvls$sex),
    `Number of deaths` = c(10, 18, 2, 22, 6, 14, 1, 9, 17, 19, 3, 11, 4, 12, 20,
                           13, 21, 5, 7, 15, 23, 16, 24, 8),
    check.names = FALSE
  )

  output <- format_uk_dths_ctry_age_sex(data)

  expect_equal(output, expected)
})
