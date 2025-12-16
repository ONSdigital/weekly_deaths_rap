test_that("Group count without groupings", {

  df <- data.frame(
    week = c(1, 1, 2, 3),
    other = 1:4
  )

  expected <- data.frame(
    week = 1:3,
    deaths = c(2, 1, 1),
    area = rep("England, Wales and non-residents", 3)
  )

  actual <- group_count(df)
  expect_equal(actual, expected)
})

test_that("Group count with region", {

  df <- data.frame(
    week = c(1, 1, 2, 2, 2, 2),
    region = c("a", "a", "a", "b", "Wales", "Nonresident"),
    other = 1:6
  )

  expected <- data.frame(
    week = c(1, 2, 2),
    area = c("a", "a", "b"),
    deaths = c(2, 1, 1)
  )

  actual <- group_count(df, "region")
  expect_equal(actual, expected)
})

test_that("Group count with country", {

  df <- data.frame(
    week = c(1, 1, 2, 2, 2),
    country = c("a", "a", "a", "b", "Nonresident"),
    other = 1:5
  )

  expected <- data.frame(
    week = c(1, 2, 2),
    area = c("a", "a", "b"),
    deaths = c(2, 1, 1)
  )

  actual <- group_count(df, "country")
  expect_equal(actual, expected)
})

test_that("Group count with groupings", {

  df <- data.frame(
    week = c(rep(1, 5), 2),
    sex = c(rep("f", 3), rep("m", 3)),
    age = rep(c(1, 1, 2), 2),
    other = 1:6
  )

  expected <- data.frame(
    week = c(1, 1, 1, 2),
    sex = c("f", "f", "m", "m"),
    age = c(1, 2, 1, 2),
    deaths = c(2, 1, 2, 1),
    area = rep("England, Wales and non-residents", 4)
  )

  actual <- group_count(df, c("sex", "age"))
  expect_equal(actual, expected)
})

test_that("Group count all geographies with groupings", {

  df <- data.frame(
    week = c(rep(1, 7), 2),
    sex = c(rep("f", 6), rep("m", 2)),
    age = c(rep(1, 3), rep(2, 5)),
    region = c("a", "a", rep("b", 3), "c", "b", "b"),
    country = c(rep("x", 5), "y", "x", "x"),
    other = 1:8
  )

  expected <- data.frame(
    week = c(1, 1, 1, 2, rep(1, 4), 2, rep(1, 5), 2),
    sex = c("f", "f", "m", "m", rep("f", 2), "m", "f", "m", rep("f", 3),
            "m", "f", "m"),
    age = c(1, rep(2, 3), 1, rep(2, 4), 1, 1, rep(2, 4)),
    deaths = c(3, 3, 1, 1, 3, 2, 1, 1, 1, 2, 1, 2, 1, 1, 1),
    area = c(rep("England, Wales and non-residents", 4),
             "x", "x", "x", "y", "x", "a", "b", "b", "b", "c", "b")
  )

  actual <- group_count_all_geo(df, c("sex", "age"))
  expect_equal(actual, expected)
})

test_that("Group count all geographies without groupings", {

  df <- data.frame(
    week = c(rep(1, 7), 2),
    sex = c(rep("f", 6), rep("m", 2)),
    age = c(rep(1, 3), rep(2, 5)),
    region = c("a", "a", rep("b", 3), "c", "b", "b"),
    country = c(rep("e", 5), "w", "e", "e"),
    other = 1:8
  )

  expected <- data.frame(
    week = c(1, 2, 1, 1, 2, 1, 1, 1, 2),
    deaths = c(7, 1, 6, 1, 1, 2, 4, 1, 1),
    area = c(rep("England, Wales and non-residents", 2),
             "e", "w", "e", "a", "b", "c", "b")
  )

  actual <- group_count_all_geo(df)
  expect_equal(actual, expected)
})

test_that("Output of format_dths_geog_age_sex_pod_imd is correct", {

  levels <- create_factor_levels()

  data <- data.frame(
    week = c(rep(1, 21), 2, 2),
    week_ending = c(rep("28 November 2023", 21), rep("17 March 2023", 2)),
    area = c("Wales", rep("England, Wales and non-residents", 7),
             rep("England", 7), rep("North East", 8)),
    sex = c("All people", rep(c("All people", rep(c("Female", "Male"), 2),
                                rep("All people", 2)), 3), "Male"),
    agegrp_5yr = c("All ages", rep(c(rep("All ages", 3), rep("1 to 4", 3),
                                     "All ages"), 3), "1 to 4"),
    place_of_death = c(
      "All places", rep(c(rep("All places", 6), "Hospital"), 3), "Hospital"),
    imd = c("Quintile 1", rep("All groups", 8),
            c(paste0("Decile ", c(rep(1, 4), 4)), rep("All groups", 9))),
    deaths = 1:23
  )

  expected <- data.frame(
    `Week number` = c(2, 2, rep(1, 21)),
    `Week ending` = c(rep("17 March 2023", 2), rep("28 November 2023", 21)),
    `Area of usual residence` = factor(
      c("North East", "North East", "England, Wales and non-residents",
        "England", "North East",
        rep(c("England, Wales and non-residents", "North East"), 5),
        rep("England", 5), "Wales", "England, Wales and non-residents",
        "England"),
      levels = levels$country_region),
    Sex = factor(
      c("All people", "Male", "All people",
        rep(c(rep("All people", 2), rep("Female", 2), rep("Male", 2)), 2),
        rep(c("Female", "Male"), 2), rep("All people", 4)),
      levels = levels$sex),
    `Age group (years)` = factor(
      c("All ages", "1 to 4", rep("All ages", 7), rep("1 to 4", 6), "All ages",
        "All ages", rep("1 to 4", 3), rep("All ages", 3)),
      levels = levels$agegrp_5yr),
    `IMD quantile group` = c(
      rep("All groups", 15), rep("Decile 1 - most deprived", 4), "Decile 4",
        "Quintile 1 - most deprived", "All groups", "All groups"),
    `Place of occurrence` = factor(
      c("Hospital", "Hospital", rep("All places", 19), rep("Hospital", 2)),
      levels = levels$place_of_death),
    `Number of deaths` = c(22, 23, 2, 9, 16, 3, 17, 4, 18, 7, 21, 5, 19, 6, 20,
                           10, 11, 12, 13, 14, 1, 8, 15),
    check.names = FALSE
  )

  actual <- format_dths_geog_age_sex_pod_imd(data)
  expect_equal(actual, expected)
})

test_that("Output of add_zero_counts function is correct", {
  # not much point writing out long "expected" data frame as original
  # tidyr::complete function already unit testes, so just checking row numbers

  data <- data.frame(
    week = c(1, 1, 2, 2),
    area = factor(
      rep(c("a", "b"), 2),
      levels = c("a", "b")),
    sex = factor(
      rep("All people", 4),
      levels = c("All people", "m", "f")),
    agegrp_5yr = factor(
      rep("All ages", 4),
      levels = c("All ages", "1", "2")),
    place_of_death = factor(
      rep("All places", 2),
      levels = c("All places", "x", "y")),
    deaths = 1:4
  )

  actual <- add_zero_counts(data)

  expect_equal(nrow(actual), 44)
  expect_equal(sum(actual$deaths), sum(data$deaths))
})

test_that("Missing week_ending filled from week", {

  data <- data.frame(
    week = c(1, 1, 2, 2),
    week_ending = c("25 December 2023", NA, NA, "2 January 2024"),
    other_col = 1:4
  )

  expected <- data.frame(
    week = c(1, 1, 2, 2),
    other_col = 1:4,
    week_ending = c("25 December 2023", "25 December 2023", "2 January 2024",
                    "2 January 2024")
  )

  expect_equal(week_ending_from_week(data), expected)
})
