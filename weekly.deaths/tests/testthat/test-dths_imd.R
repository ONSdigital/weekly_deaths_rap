test_that("Creation of imd count table for decile/England/registrations", {

  imd_vals <- paste0("Decile ", 1:10)
  lvls_age <- create_factor_levels()$agegrp_5yr[-1]

  dummy_imd <- data.frame(
    lsoa11cd = c("E001", paste0("E0", 1:10)),
    imd_decile = c(1, 1:10)
  )

  dummy_data <- data.frame(
    country = c(rep("England", 16), rep("Wales", 16)),
    lsoa11 = c("E001", paste0("E0", 1:10), paste0("E0", 1:5), rep("W01", 16)),
    week = rep(c(rep(1, 8), rep(2, 8)), 2),
    week_ending = rep(c(rep("6 January", 8), rep("13 January", 8)), 2),
    year = rep(2024, 32),
    agegrp_5yr = c(rep("50 to 54", 5), rep("60 to 64", 27))
  )

  expected <- data.frame(
    week = c(rep(1, 220), rep(2, 220), rep(1, 10), rep(2, 10)),
    week_ending = c(rep("6 January", 220), rep("13 January", 220),
                    rep("6 January", 10), rep("13 January", 10)),
    agegrp_5yr = c(rep(rep(lvls_age, each = 10), 2), rep("All ages", 20)),
    imd = factor(rep(imd_vals, 46), imd_vals),
    count = c(rep(0, 110), 2, 1, 1, 1, rep(0, 20), 1, 1, 1, rep(0, 213),
              rep(1, 5), 0, 0, 1, 1, 1, rep(0, 80), 2, rep(1, 6), 0, 0, 0,
              rep(1, 5), 0, 0, 1, 1, 1),
    area = rep("England", 460),
    check.names = FALSE
  )

  class(expected) <- c("tbl_df", "tbl", "data.frame")
  expect_equal(count_imd(dummy_data, 2, 2024, dummy_imd, "decile", "England"),
               expected)
})

test_that("Creation of imd count table for quintile/Wales", {

  imd_vals <- paste0("Quintile ", 1:5)
  lvls_age <- create_factor_levels()$agegrp_5yr[-1]

  dummy_imd <- data.frame(
    lsoa11cd = c("W001", paste0("W0", 1:5)),
    imd_quintile = c(1, 1:5)
  )

  dummy_data <- data.frame(
    country = c("England", rep("Wales", 11)),
    lsoa11 = c(
      "E01", "W001", "W01", "W01", "W02", paste0("W0", 1:5), rep("W01", 2)),
    week = c(rep(1:2, 5), 3, 2),
    week_ending = rep(c("6 January", "13 January"), 6),
    agegrp_5yr = rep("Under 1", 12),
    year = c(rep(2024, 11), 2023)
  )

  expected <- data.frame(
    week = c(rep(1, 110), rep(2, 110), rep(1, 5), rep(2, 5)),
    week_ending = c(rep("6 January", 110), rep("13 January", 110),
                    rep("6 January", 5), rep("13 January", 5)),
    agegrp_5yr = c(rep(rep(lvls_age, each = 5), 2), rep("All ages", 10)),
    imd = factor(rep(imd_vals, 46), imd_vals),
    count = c(1, 2, 0, 1, rep(0, 106), 3, 0, 1, 0, 1, rep(0, 105), 1, 2, 0, 1,
              0, 3, 0, 1, 0, 1),
    area = rep("Wales", 230),
    check.names = FALSE)

  class(expected) <- c("tbl_df", "tbl", "data.frame")
  expect_equal(count_imd(dummy_data, 2, 2024, dummy_imd, "quintile", "Wales"),
               expected)

})

test_that("IMD table creation func gives error for incorrect args", {

  dummy_imd <- data.frame(
    lsoa11cd = c("W001", paste0("W0", 1:5)),
    imd_quintile = c(1, 1:5)
  )

  dummy_data <- data.frame(
    country = c("England", rep("Wales", 9)),
    lsoa11 = c("E01", "W001", "W01", "W01", "W02", paste0("W0", 1:5)),
    week = rep(1:2, 5),
    week_ending = rep(c("6 January", "13 January"), 5),
    agegrp_5yr = rep("Under 1", 10),
    year = rep(2024, 10)
  )


  expect_error(count_imd(
    dummy_data, 10, 2024, dummy_imd, "xyz", "England"),
    'quantile argument must be "decile" or "quintile"')
  expect_error(count_imd(
    dummy_data, 10, 2024, dummy_imd, "decile", "xyz"),
    'country argument must be "England" or "Wales"')

})

test_that("Binding of English and Welsh IMD data frames onto table data", {

  lvls_age <- create_factor_levels()$agegrp_5yr[-1]

  dummy_table_data <- data.frame(
    week = c(2, 1),
    area = rep("England, Wales and non-residents", 2),
    sex = rep("Female", 2),
    agegrp_5yr = rep("All people", 2),
    place_of_death = rep("Home", 2),
    deaths = 1:2,
    week_ending = c("12 January", "5 January")
  )

  dummy_deaths_data <- data.frame(
    country = c("England", "Wales"),
    lsoa11 = c("E01", "W01"),
    week = c(1, 2),
    week_ending = c("5 January", "12 January"),
    agegrp_5yr = rep("Under 1", 2),
    year = c(2024, 2024)
  )

  dummy_imd <- list(
    wales = data.frame(
      lsoa11cd = paste0("W0", 1:5),
      imd_quintile = 1:5
    ),
    england = data.frame(
      lsoa11cd = paste0("E0", 1:10),
      imd_decile = 1:10
    )
  )

  expected <- data.frame(
    week = c(2, 1, rep(1, 230), rep(2, 115)),
    area = c(rep("England, Wales and non-residents", 2), rep("England", 230),
             rep("Wales", 115)),
    sex = c(rep("Female", 2), rep("All people", 345)),
    agegrp_5yr = c(rep("All people", 2), rep(lvls_age, each = 10),
                   rep("All ages", 10), rep(lvls_age, each = 5),
                   rep("All ages", 5)),
    place_of_death = c(rep("Home", 2), rep("All places", 345)),
    deaths = c(1:2, 1, rep(0, 219), 1, rep(0, 9), 1, rep(0, 109), 1, rep(0, 4)),
    week_ending = c(
      "12 January", rep("5 January", 231), rep("12 January", 115)),
    imd = c("All groups", "All groups", rep(paste0("Decile ", 1:10), 23),
            rep(paste0("Quintile ", 1:5), 23))
  )

  actual <- bind_imd_values(dummy_table_data, dummy_deaths_data, 2, 2024,
                            dummy_imd)

  expect_equal(expected, actual)

})

test_that("Adding most/least deprived onto IMD decile group column", {

  dummy_data <- data.frame(
    col_1 = 1:6,
    `IMD quantile group` = c("All groups", "Decile 5", "Decile 1", "Decile 10",
                             "Quintile 5", "Quintile 1"),
    check.names = FALSE
  )

  expected <- data.frame(
    col_1 = 1:6,
    `IMD quantile group` = c(
      "All groups", "Decile 5", "Decile 1 - most deprived",
      "Decile 10 - least deprived", "Quintile 5 - least deprived",
      "Quintile 1 - most deprived"),
    check.names = FALSE
  )

  expect_equal(add_most_least_deprived_label(dummy_data), expected)
})
