test_that("Process pops into totals by country (inc UK), agegrp, sex", {

  alph_ctry <- c("England", "England, Wales and non-residents",
                 "Northern Ireland", "Scotland", "UK", "Wales")
  orig_ctry <- c("England, Wales and non-residents", "England", "Wales",
                 "Scotland", "Northern Ireland")

  alph_agrp <- c("1 to 4", "85 to 89", "90 and over", "Under 1")
  orig_agrp <- c("Under 1", "1 to 4", "85 to 89", "90 and over")

  data <- data.frame(
    dor_year = c(rep(2025, 80), 2024),
    dor_week = c(rep(rep(c(1:2), each = 8), 5), 1),
    esp_age_group = c(
      rep(rep(c("<1", "01-04", "85-89", "90+"), each = 2), 10), "<1"),
    sex = c(rep(c("Female", "Male"), 40), "Male"),
    population = c(rev(seq(10, 800, 10)), 10),
    area = c(rep(c(orig_ctry), 16), "England, Wales and non-residents"),
    other_col = rep("other", 81)
  )

  expected <- data.frame(
    year = rep(2025, 180),
    week = c(rep(rep(c(1:2), each = 8), 6), rep(c(1:2), each = 12),
             rep(1:2, each = 24), rep(1:2, each = 6)),
    country = c(rep(orig_ctry, 16), rep("UK", 16), rep(alph_ctry, 4),
                rep(rep(alph_ctry, each = 4), 2), rep(alph_ctry, 2)),
    agegroup_2 = c(rep(rep(orig_agrp, each = 2), 10), rep(alph_agrp, 4),
                   rep("All ages", 24), rep(alph_agrp, 12),
                   rep("All ages", 12)),
    sex = c(rep(c("Female", "Male"), 40),
            rep(rep(c("Female", "Male"), each = 4), 2),
            rep(rep(c("Female", "Male"), each = 6), 2), rep("All people", 60)),
    population =
      c(800, 790, 780, 770, 760, 750, 740, 730, 720, 710, 700, 690, 680, 670,
        660, 650, 640, 630, 620, 610, 600, 590, 580, 570, 560, 550, 540, 530,
        520, 510, 500, 490, 480, 470, 460, 450, 440, 430, 420, 410, 400, 390,
        380, 370, 360, 350, 340, 330, 320, 310, 300, 290, 280, 270, 260, 250,
        240, 230, 220, 210, 200, 190, 180, 170, 160, 150, 140, 130, 120, 110,
        100, 90, 80, 70, 60, 50, 40, 30, 20, 10, 1380, 1480, 780, 1280, 1830,
        1130, 1230, 930, 980, 1080, 1180, 1680, 630, 1530, 830, 1330, 1960,
        1800, 1640, 1480, 4920, 2120, 1760, 1600, 1440, 2080, 5120, 1920, 1160,
        1800, 1640, 1480, 4920, 1320, 1760, 1600, 1440, 1280, 4320, 1120, 430,
        1030, 830, 1430, 750, 1350, 350, 950, 1070, 870, 670, 470, 1390, 390,
        990, 790, 3210, 2610, 2010, 2210, 910, 710, 1310, 1110, 1230, 230, 830,
        630, 750, 550, 1150, 950, 270, 870, 670, 1270, 590, 1190, 190, 790,
        1610, 2610, 2010, 3010, 910, 710, 510, 310, 3720, 3400, 3080, 3560,
        10040, 4040, 2920, 3400, 3080, 2760, 9240, 2440)
  )

  actual <- population_processing_totals(data,
                                         current_year = 2025)

  expect_equal(actual, expected, ignore_attr = TRUE)
})


test_that("Calculate ASMR works correctly", {
  df <- data.frame(
    year = rep(rep(c(2025, 2024), each = 6), 8),
    week = rep(rep(1:2, each = 12), 4),
    country = rep(rep(c("England", "Wales"), each = 24), 2),
    sex = rep(c("Male", "Female"), each = 48),
    age_group =
      rep(c("All ages", "Under 1", "1 to 19", "20 to 59", "60 to 89",
          "90 and over"), 16),
    deaths =
      c(377, 23, 53, 93, 107, 101, 397, 27, 57, 97, 111, 105, 382, 24, 54, 94,
        108, 102, 402, 28, 58, 98, 112, 106, 68, 0, 3, 7, 37, 21, 83, 3, 6, 10,
        40, 24, 78, 2, 5, 9, 39, 23, 93, 5, 8, 12, 42, 26, 377, 23, 53, 93, 107,
        101, 397, 27, 57, 97, 111, 105, 382, 24, 54, 94, 108, 102, 402, 28, 58,
        98, 112, 106, 68, 0, 3, 7, 37, 21, 83, 3, 6, 10, 40, 24, 78, 2, 5, 9,
        39, 23, 93, 5, 8, 12, 42, 26),
    population = rep(c(rep(c(29000, 1000, 5000, 10000, 8000, 5000), 4),
                   rep(c(14500, 500, 2500, 5000, 4000, 2500), 4)), 2),
    esp = rep(c(100000, 1000, 20500, 53000, 24500, 1000), 16),
    other = 1:96
  )

  expected <- data.frame(
    year = rep(2024:2025, each = 8),
    week = rep(rep(1:2, each = 4), 2),
    country = rep(rep(c("England", "Wales"), each = 2), 4),
    sex = rep(c("Female", "Male"), 8),
    deaths = rep(c(397, 83, 402, 93, 377, 68, 382, 78), each = 2),
    age_standardised_rate =
      rep(c(1135.738, 415.800, 1149.400, 470.450, 1081.088, 333.825, 1094.75,
            388.475), each = 2)
  )

  class(expected) <- c("tbl_df", "tbl", "data.frame")

  output <- calculate_asmr(df,
                          grouping_vars  = c("year",
                                             "week",
                                             "country",
                                             "sex")) %>%
    dplyr::select(-ci_lower, -ci_upper) %>%
    dplyr::mutate(age_standardised_rate = round(age_standardised_rate, 3))

  expect_equal(output, expected)
})


test_that("Calculate ASMR works correctly, NA in esp", {
  df <- data.frame(
    year = rep(2025, 6),
    week = rep(1, 6),
    country = rep("England", 6),
    sex = rep("Male", 6),
    agegroup_2 =
      c("All ages", "Under 1", "1 to 19", "20 to 59", "60 to 89",
        "90 and over"),
    deaths = c(377, 23, 53, 93, 107, 101),
    population = c(29000, 1000, 5000, 10000, 8000, 5000),
    esp = c(100000, 1000, 20500, 53000, 24500, NA),
    other = 1:6
  )

  expect_error(calculate_asmr(df),
               "There must be no missing/NA in the dataframe, check data.")
})

test_that("Calculate ASMR works correctly, NA in deaths", {
  df <- data.frame(
    year = rep(2025, 6),
    week = rep(1, 6),
    country = rep("England", 6),
    sex = rep("Male", 6),
    agegroup_2 =
      c("All ages", "Under 1", "1 to 19", "20 to 59", "60 to 89",
        "90 and over"),
    deaths = c(377, 23, 53, 93, 107, NA),
    population = c(29000, 1000, 5000, 10000, 8000, 5000),
    esp = c(100000, 1000, 20500, 53000, 24500, 1000),
    other = 1:6
  )

  expect_error(calculate_asmr(df),
               "There must be no missing/NA in the dataframe, check data.")
})

test_that("Calculate ASMR works correctly, NA in pop", {
  df <- data.frame(
    year = rep(2025, 6),
    week = rep(1, 6),
    country = rep("England", 6),
    sex = rep("Male", 6),
    agegroup_2 =
      c("All ages", "Under 1", "1 to 19", "20 to 59", "60 to 89",
        "90 and over"),
    deaths = c(377, 23, 53, 93, 107, 101),
    population = c(29000, 1000, 5000, 10000, 8000, 5000),
    esp = c(100000, 1000, 20500, 53000, 24500, NA),
    other = 1:6
  )

  expect_error(calculate_asmr(df),
               "There must be no missing/NA in the dataframe, check data.")
})

test_that("Calculate ASMR still calculates, not all var combo present", {
  # If there are only some factor level combinations, still calculates it
  df <- data.frame(
    year = rep(2025, 25),
    week = rep(1, 25),
    country =
      c(rep("England", 5), rep(c("Wales", "England"), each = 4),
        rep("Wales", 2), rep("England", 3), rep("Wales", 2),
        rep(c("England", "Wales"), 2), "England"),
    sex =
      c(rep(c("Male", "Female"), 2), rep("Male", 2), "Female", "All people",
        rep(c("Female", "Male"), each = 2), "Male", rep("All people", 2),
        "Female", "Male", "All people", "Female", "All people", "Male",
        rep("All people", 2), "Male", "All people"),
    age_group =
      c("Under 1", "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",
        "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
        "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",
        "85 and over", rep("All ages", 6)),
    deaths = c(52, 173, 196, 17, 78, 115, 149, 157, 147, 103,
               166, 99, 76, 81, 154, 90, 151, 112, 18, 47,
               32, 125, 151, 32, 150),
    population =
      c(18935226, 25165727, 11809345, 42658839, 3875308, 12897462, 12965432,
        37404585, 4352859, 24435049, 43943264, 10710284, 27259910, 8317403,
        37629527, 13389244, 37456705, 30888293, 41356435, 17222254, 13608723,
        46326685, 7679363, 38734659, 46913845),
    esp =
      c(47064, 37634, 31611, 37720, 49249, 33975, 12208, 26723, 20969, 25309,
        41202, 16050, 42329, 17492, 45427, 26183, 44480, 33330, 35087, 43343,
        14833, 37887, 11123, 31655, 24734)
  )

  expected <- data.frame(
    year = rep(2025, 6),
    week = rep(1, 6),
    country = rep(c("England", "Wales"), each = 3),
    sex = rep(c("All people", "Female", "Male"), 2),
    deaths = c(112, 383, 818, 392, 314, 115),
    age_standardised_rate =
      c(0.36260, 0.43866, 0.82596, 0.52255, 1.26525, 0.89165)
  )

  actual <- calculate_asmr(df, c("year", "week", "country", "sex")) %>%
    dplyr::select(-ci_lower, -ci_upper) %>%
    dplyr::mutate(age_standardised_rate = round(age_standardised_rate, 5))

  class(expected) <- c("tbl_df", "tbl", "data.frame")

  expect_equal(actual, expected)
})

test_that("Convert weekly ASMR to annual ASMR", {

  age_standardised_rate <- c(70, 77, 1000, 123.45, 98765.5, NA)

  # Non-leap year
  expect_equal(convert_to_yearly_asmr(age_standardised_rate, 2025),
               c(3650, 4015, 52142.9, 6437.0, 5149915.4, NA))

  # Leap year
  expect_equal(convert_to_yearly_asmr(age_standardised_rate, 2024),
               c(3660, 4026, 52285.7, 6454.7, 5164024.7, NA))
})

test_that("Check formatting of ASMR", {

  lvls <- create_factor_levels()

  data <- data.frame(
    country = rep(c("England", "England, Wales and non-residents",
                "Northern Ireland", "Scotland", "UK", "Wales"), 6),
    sex = rep(rep(c("Male", "Female", "All people"), each = 6), 2),
    week = rep(1:2, each = 18),
    year = rep(2025, 36),
    age_standardised_rate = c(seq(1000.0, 1003.5, 0.1)),
    deaths = rep(100, 36),
    ci_lower = c(1:36),
    ci_upper = c(37:72),
    other = c(1:36)
  )

  expected <- data.frame(
    `Week number` = rep(2:1, each = 18),
    Country = factor(rep(lvls$country, 6), levels = lvls$country),
    Sex = factor(rep(rep(lvls$sex, each = 6), 2), levels = lvls$sex),
    `ASMR per 100,000` =
      c(1003.4, 1003.1, "1003.0", 1003.5, 1003.3, 1003.2,
        1002.8, 1002.5, 1002.4, 1002.9, 1002.7, 1002.6,
        1002.2, 1001.9, 1001.8, 1002.3, 1002.1, "1002.0",
        1001.6, 1001.3, 1001.2, 1001.7, 1001.5, 1001.4,
        "1001.0", 1000.7, 1000.6, 1001.1, 1000.9, 1000.8,
        1000.4, 1000.1, "1000.0", 1000.5, 1000.3, 1000.2),
    `Lower confidence limit` =
      c("35.0", "32.0", "31.0", "36.0", "34.0", "33.0", "29.0", "26.0", "25.0",
        "30.0", "28.0", "27.0", "23.0", "20.0", "19.0", "24.0", "22.0", "21.0",
        "17.0", "14.0", "13.0", "18.0", "16.0", "15.0", "11.0", "8.0", "7.0",
        "12.0", "10.0", "9.0", "5.0", "2.0", "1.0", "6.0", "4.0", "3.0"),
    `Upper confidence limit` =
      c("71.0", "68.0", "67.0", "72.0", "70.0", "69.0", "65.0", "62.0", "61.0",
        "66.0", "64.0", "63.0", "59.0", "56.0", "55.0", "60.0", "58.0", "57.0",
        "53.0", "50.0", "49.0", "54.0", "52.0", "51.0", "47.0", "44.0", "43.0",
        "48.0", "46.0", "45.0", "41.0", "38.0", "37.0", "42.0", "40.0",
        "39.0"),
    check.names = FALSE
  )

  actual <- format_asmr(data)

  expect_equal(expected, actual)
})

test_that("Check formatting of ASMR, suppress rates when deaths < 10", {

  lvls <- create_factor_levels()

  data <- data.frame(
    country = rep(c("England", "England, Wales and non-residents",
                    "Northern Ireland", "Scotland", "UK", "Wales"), 6),
    sex = rep(rep(c("Male", "Female", "All people"), each = 6), 2),
    week = rep(1:2, each = 18),
    year = rep(2025, 36),
    age_standardised_rate = c(seq(1000.0, 1003.5, 0.1)),
    deaths = c(9, rep(100, 35)),
    ci_lower = c(1:36),
    ci_upper = c(37:72),
    other = c(1:36)
  )

  expected <- data.frame(
    `Week number` = rep(2:1, each = 18),
    Country = factor(rep(lvls$country, 6), levels = lvls$country),
    Sex = factor(rep(rep(lvls$sex, each = 6), 2), levels = lvls$sex),
    `ASMR per 100,000` =
      c(1003.4, 1003.1, "1003.0", 1003.5, 1003.3, 1003.2,
        1002.8, 1002.5, 1002.4, 1002.9, 1002.7, 1002.6,
        1002.2, 1001.9, 1001.8, 1002.3, 1002.1, "1002.0",
        1001.6, 1001.3, 1001.2, 1001.7, 1001.5, 1001.4,
        "1001.0", 1000.7, 1000.6, 1001.1, 1000.9, 1000.8,
        1000.4, 1000.1, "[x]", 1000.5, 1000.3, 1000.2),
    `Lower confidence limit` =
      c("35.0", "32.0", "31.0", "36.0", "34.0", "33.0", "29.0", "26.0", "25.0",
        "30.0", "28.0", "27.0", "23.0", "20.0", "19.0", "24.0", "22.0", "21.0",
        "17.0", "14.0", "13.0", "18.0", "16.0", "15.0", "11.0", "8.0", "7.0",
        "12.0", "10.0", "9.0", "5.0", "2.0", "[x]", "6.0", "4.0", "3.0"),
    `Upper confidence limit` = as.character(
      c("71.0", "68.0", "67.0", "72.0", "70.0", "69.0", "65.0", "62.0", "61.0",
        "66.0", "64.0", "63.0", "59.0", "56.0", "55.0", "60.0", "58.0", "57.0",
        "53.0", "50.0", "49.0", "54.0", "52.0", "51.0", "47.0", "44.0", "43.0",
        "48.0", "46.0", "45.0", "41.0", "38.0", "[x]", "42.0", "40.0", "39.0")),
    check.names = FALSE
  )

  actual <- format_asmr(data)

  expect_equal(expected, actual)
})

test_that("Check formatting of ASMR, warn for NA (wrong factor levels)", {

  data <- data.frame(
    country = rep(c("England", "England, Wales and non-residents",
                    "Northern Ireland", "Scotland", "United Kingdom",
                    "Wales"), 6),
    sex = rep(rep(c("Male", "Female", "All people"), each = 6), 2),
    week = rep(1:2, each = 18),
    year = rep(2025, 36),
    age_standardised_rate = c(seq(1000.0, 1003.5, 0.1)),
    deaths = rep(100, 36),
    ci_lower = c(1:36),
    ci_upper = c(37:72),
    other = c(1:36)
  )

  expect_error(
    format_asmr(data),
    "There will be unexpected NA in the ASMR table. Check the data.")

  data <- data.frame(
    country = rep(c("England", "England, Wales and non-residents",
                    "Northern Ireland", "Scotland", "United Kingdom",
                    "Wales"), 6),
    sex = rep(rep(c("Male", "Female", "All people"), each = 6), 2),
    week = rep(1:2, each = 18),
    year = rep(2025, 36),
    age_standardised_rate = c(NA, seq(1000.0, 1003.4, 0.1)),
    deaths = rep(100, 36),
    ci_lower = c(1:36),
    ci_upper = c(37:72),
    other = c(1:36)
  )

  expect_error(
    format_asmr(data),
    "There will be unexpected NA in the ASMR table. Check the data.")
})

test_that("Process UK deaths for ASMR", {

  lvls <- create_factor_levels()

  ew_df <- data.frame(
    year = c(rep(2025, 21), 2024),
    week = c(rep(1:3, each = 7), 1),
    dor = c(20250101:20250121, 20240101),
    sex = rep("Female", 22),
    agegrp_5yr = lvls$agegrp_5yr[-1],
    country = rep(c("England", "Wales"), each = 11),
    other = 1:22
  )

  scot_df <- data.frame(
    year = c(rep(2025, 21), 2024),
    week = c(rep(1:2, each = 7), rep(1, 8)),
    agegroup_2 = c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                   "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                   "60-64", "65-69", "70-74", "75-79", "80-84", "85-89",
                   rep("90+", 3)),
    covid = c(rep(0, 20), 1, 0),
    dor = c(20250101:20250120, 20250101, 20240101),
    sex = rep("Male", 22),
    country = rep("Scotland", 22),
    deaths = seq(10, 220, 10),
    other = 1:22
  )

  ni_df <- data.frame(
    year = c(rep(2025, 20), 2024, 2024),
    week = c(rep(1:2, each = 7), rep(1, 8)),
    agegroup_2 = c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                   "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                   "60-64", "65-69", "70-74", "75-79", "80-84", "85-89",
                   rep("90+", 3)),
    dor = c(20250101:20250120, 20240101, 20240101),
    sex = rep("Male", 22),
    country = rep("Northern Ireland", 22),
    deaths = seq(10, 220, 10),
    other = 1:22
  )

  agegrp_0s <- c("<1", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29",
                 "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                 "60-64", "65-69", "70-74", "75-79", "80-84", "85-89",
                 "90+")

  esp <- data.frame(
    agegroup = agegrp_0s,
    esp = c(1000, 4000, rep(5500, 3), rep(6000, 2), 6500, rep(7000, 4),
            6500, 6000, 5500, 5000, 4000, 2500, 1500, 1000)
  )

  weekly_pop <- data.frame(
    dor_year = rep(c(rep(2024, 200), rep(2025, 200)), 3),
    dor_week = rep(rep(1:3, each = 400)),
    esp_age_group = rep(rep(rep(rep(agegrp_0s, 5), each = 2), 2), 3),
    sex = rep(rep(rep(c("Female", "Male"), 100), 2), 3),
    area = rep(rep(rep(lvls$country[-1], each = 40), 2), 3),
    population =
      rep(rep(c(seq(30, 1200, 30), seq(20, 800, 20), seq(10, 400, 10),
                seq(210, 600, 10), seq(110, 500, 10)), 2), 3)
  )

  sort_age <- c(lvls$agegrp_5yr[c(3, 5:12, 4, 13:20)], "90 and over",
                lvls$agegrp_5yr[c(1:2)])

  pop_eng <-
    c(140, 300, 380, 460, 540, 620, 700, 780, 860, 220, 940, 1020, 1100, 1180,
      1260, 1340, 1420, 1500, 1580, 16400, 60, 60, 140, 180, 220, 260, 300, 340,
      380, 420, 100, 460, 500, 540, 580, 620, 660, 700, 740, 780, 8000, 20, 80,
      160, 200, 240, 280, 320, 360, 400, 440, 120, 480, 520, 560, 600, 640, 680,
      720, 760, 800, 8400, 40)

  pop_wal <-
    c(70, 150, 190, 230, 270, 310, 350, 390, 430, 110, 470, 510, 550, 590, 630,
      670, 710, 750, 790, 8200, 30, 30, 70, 90, 110, 130, 150, 170, 190, 210,
      50, 230, 250, 270, 290, 310, 330, 350, 370, 390, 4000, 10, 40, 80, 100,
      120, 140, 160, 180, 200, 220, 60, 240, 260, 280, 300, 320, 340, 360, 380,
      400, 4200, 20)

  pop_scot <-
    c(470, 550, 590, 630, 670, 710, 750, 790, 830, 510, 870, 910, 950, 990,
      1030, 1070, 1110, 1150, 1190, 16200, 430, 230, 270, 290, 310, 330, 350,
      370, 390, 410, 250, 430, 450, 470, 490, 510, 530, 550, 570, 590, 8000,
      210, 240, 280, 300, 320, 340, 360, 380, 400, 420, 260, 440, 460, 480,
      500, 520, 540, 560, 580, 600, 8200, 220)

  pop_ni <-
    c(270, 350, 390, 430, 470, 510, 550, 590, 630, 310, 670, 710, 750, 790,
      830, 870, 910, 950, 990, 12200, 230, 130, 170, 190, 210, 230, 250, 270,
      290, 310, 150, 330, 350, 370, 390, 410, 430, 450, 470, 490, 6000,
      110, 140, 180, 200, 220, 240, 260, 280, 300, 320, 160, 340, 360,
      380, 400, 420, 440, 460, 480, 500, 6200, 120)

  expected <- data.frame(
    year = rep(2025, 1134),
    week = rep(1:3, each = 378),
    country = rep(rep(sort(lvls$country), each = 63), 3),
    sex = rep(rep(lvls$sex, each = 21), 18),
    age_group = rep(sort_age, 54),
    deaths =
      c(rep(1, 5), rep(0, 4), 1, rep(0, 9), 7, rep(1, 6), rep(0, 4), 1,
        rep(0, 9), 7, 1, rep(0, 21), rep(1, 5), rep(0, 4), 1, rep(0, 9), 7,
        rep(1, 6), rep(0, 4), 1, rep(0, 9), 7, 1, rep(0, 21), 20, 40, 50, 60,
        70, rep(0, 4), 30, rep(0, 3), 150, 160, 170, 180, 190, 200, 1330, 10,
        rep(0, 21), 20, 40, 50, 60, 70, rep(0, 4), 30, rep(0, 3), 150, 160,
        170, 180, 190, 200, 1330, 10, 20, 40, 50, 60, 70, rep(0, 4), 30,
        rep(0, 3), 150, 160, 170, 180, 190, 200, 1330, 10, rep(0, 21),
        20, 40, 50, 60, 70, rep(0, 4), 30, rep(0, 3), 150, 160, 170, 180, 190,
        200, 1330, 10, 41, 81, 101, 121, 141, rep(0, 4), 61, rep(0, 3), 300,
        320, 340, 360, 380, 400, 2667, 21, rep(1, 5), rep(0, 4), 1, rep(0, 9),
        7, 1, 40, 80, 100, 120, 140, rep(0, 4), 60, rep(0, 3), 300, 320, 340,
        360, 380, 400, 2660, 20, rep(0, 68), rep(1, 4), rep(0, 10), 4,
        rep(0, 6), rep(1, 4), rep(0, 10), 4, rep(0, 27), rep(1, 4), 0,
        rep(1, 3), rep(0, 6), 7, rep(0, 6), rep(1, 4), 0, rep(1, 3),
        rep(0, 6), 7, rep(0, 27), 80, 90, 100, 110, 0, 120, 130, 140,
        rep(0, 6), 770, rep(0, 27), 80, 90, 100, 110, 0, 120, 130, 140,
        rep(0, 6), 770, rep(0, 6), 80, 90, 100, 110, 0, 120, 130, 140,
        rep(0, 6), 770, rep(0, 27), 80, 90, 100, 110, 0, 120, 130, 140,
        rep(0, 6), 770, rep(0, 6), 161, 181, 201, 221, 0, 241, 261, 281,
        rep(0, 6), 1547, rep(0, 6), rep(1, 4), 0, rep(1, 3), rep(0, 6), 7,
        rep(0, 6), 160, 180, 200, 220, 0, 240, 260, 280, rep(0, 6), 1540,
        rep(0, 11), rep(1, 3), rep(0, 6), 3, rep(0, 11), rep(1, 3),
        rep(0, 6), 3, rep(0, 98), rep(1, 5), 2, 7, rep(0, 14), rep(1, 5),
        2, 7, rep(0, 161), rep(1, 5), 2, 7, rep(0, 14), rep(1, 5), 2, 7,
        rep(0, 35), rep(1, 5), 2, 7, rep(0, 14), rep(1, 5), 2, 7,
        rep(0, 22)),
    population =
      rep(c(pop_eng, (pop_eng + pop_wal), pop_ni, pop_scot,
            (pop_eng + pop_wal + pop_ni + pop_scot), pop_wal), 3),
    esp =
      c(4000, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000, 5500,
        7000, 6500, 6000, 5500, 5000, 4000, 2500, 1500, 1000, 100000, 1000))

  output <- process_uk_deaths_asmr(ew_df,
                                   scot_df,
                                   ni_df,
                                   current_year = 2025,
                                   current_week = 2,
                                   scope = "UK",
                                   esp,
                                   weekly_pop)

  class(expected) <- c("tbl_df", "tbl", "data.frame")

  expect_equal(output, expected)
})


test_that("Calculate ASMR with CI works correctly", {
  df <- data.frame(
    year = rep(rep(c(2025, 2024), each = 6), 8),
    week = rep(rep(1:2, each = 12), 4),
    country = rep(rep(c("England", "Wales"), each = 24), 2),
    sex = rep(c("Male", "Female"), each = 48),
    age_group =
      rep(c("All ages", "Under 1", "1 to 19", "20 to 59", "60 to 89",
            "90 and over"), 16),
    deaths =
      c(377, 23, 53, 93, 107, 101, 397, 27, 57, 97, 111, 105, 382, 24, 54, 94,
        108, 102, 402, 28, 58, 98, 112, 106, 68, 0, 3, 7, 37, 21, 83, 3, 6, 10,
        40, 24, 78, 2, 5, 9, 39, 23, 93, 5, 8, 12, 42, 26, 377, 23, 53, 93, 107,
        101, 397, 27, 57, 97, 111, 105, 382, 24, 54, 94, 108, 102, 402, 28, 58,
        98, 112, 106, 68, 0, 3, 7, 37, 21, 83, 3, 6, 10, 40, 24, 78, 2, 5, 9,
        39, 23, 93, 5, 8, 12, 42, 26),
    population = rep(c(rep(c(29000, 1000, 5000, 10000, 8000, 5000), 4),
                       rep(c(14500, 500, 2500, 5000, 4000, 2500), 4)), 2),
    esp = rep(c(100000, 1000, 20500, 53000, 24500, 1000), 16),
    other = 1:96
  )

  expected <- data.frame(
    year = rep(2024:2025, each = 8),
    week = rep(rep(1:2, each = 4), 2),
    country = rep(rep(c("England", "Wales"), each = 2), 4),
    sex = rep(c("Female", "Male"), 8),
    deaths = rep(c(397, 83, 402, 93, 377, 68, 382, 78), each = 2),
    age_standardised_rate =
      rep(c(1135.7, 415.8, 1149.4, 470.5, 1081.1, 333.8, 1094.8,
            388.5), each = 2),
    ci_lower = rep(c(1003.9, 313.5, 1016.8, 360.6, 952.4, 243.9, 965.3, 290.1),
                   each = 2),
    ci_upper = rep(c(1277.8, 536.3, 1292.2, 598.6, 1220.0, 441.5, 1234.4,
                     504.9),
                   each = 2)
  )

  class(expected) <- c("tbl_df", "tbl", "data.frame")

  output <- calculate_asmr(df,
                          grouping_vars  = c("year",
                                             "week",
                                             "country",
                                             "sex")) %>%
    dplyr::mutate(age_standardised_rate = round(age_standardised_rate, 1),
                  ci_lower = round(ci_lower, 1),
                  ci_upper = round(ci_upper, 1))

  expect_equal(output, expected)
})
