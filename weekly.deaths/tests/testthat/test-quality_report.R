test_that("Get info for checking excess deaths subtotals for age", {

  dummy_data_df <- data.frame(
    dor_week = c(1, 2, 3),
    country = c("England, Wales and non-residents",
                                "England", "Wales"),
    sex = rep("All People", 3),
    expected_deaths = c(10000, 9500, 450)
  )
  dummy_x <- "ew_excess_deaths"
  dummy_topic <- "age"
  lvls <- create_factor_levels()
  expected <- list(
    var = "age_group_years",
    groupings = lvls$agegrp_excess[lvls$agegrp_excess != "All ages"],
    vars = c("country", "sex"),
    overall = "All ages",
    data = data.frame(
      dor_week = 1,
      country = "England, Wales and non-residents",
      sex = "All People",
      expected_deaths = 10000
    )
  )
  expect_equal(
    get_excess_subgroup_info(dummy_data_df, "ew_excess_deaths", "age"),
    expected)
})


test_that("compare_totals gives TRUE when totals match for registrations", {

  dummy_data_df <- data.frame(
    week_number = rep(1, 6),
    age = c(rep("All ages", 4), rep("0 to 29", 4), rep("30 to 44", 4)),
    area_of_usual_residence = rep(c(rep("England, Wales and non-residents", 2),
                                    rep("England", 2)), 3),
    sex = rep(c("Female", "Male"), 6),
    number_of_deaths = c(10, 12, 7, 8, 6, 7, 5, 7, 4, 5, 2, 1)
  )
  expected <- compare_totals(dummy_data_df, "registrations", "age",
                             c("0 to 29", "30 to 44"),
                             c("sex", "area_of_usual_residence"),
                             "All ages")
  expect_equal(expected, TRUE)
})

test_that("compare_totals gives TRUE when totals match for occurrences", {

  dummy_df_2024 <- data.frame(
    year = rep(2024, 6),
    week_number = rep(1, 6),
    age = c(rep("All ages", 4), rep("0 to 29", 4), rep("30 to 44", 4)),
    area_of_usual_residence = rep(c(rep("England, Wales and non-residents", 2),
                               rep("England", 2)), 3),
    sex = rep(c("Female", "Male"), 6),
    number_of_deaths = c(10, 12, 7, 8, 6, 7, 5, 7, 4, 5, 2, 1)
  )

  dummy_df_2023 <- data.frame(
    year = rep(2023, 6),
    week_number = rep(1, 6),
    age = c(rep("All ages", 4), rep("0 to 29", 4), rep("30 to 44", 4)),
    area_of_usual_residence = rep(c(rep("England, Wales and non-residents", 2),
                                    rep("England", 2)), 3),
    sex = rep(c("Female", "Male"), 6),
    number_of_deaths = c(10, 12, 7, 8, 6, 7, 5, 7, 4, 5, 2, 1)
  )

  dummy_df_comb <- rbind(dummy_df_2024, dummy_df_2023)

  expected <- compare_totals(dummy_df_comb, "occurrences", "age",
                             c("0 to 29", "30 to 44"),
                             c("sex", "area_of_usual_residence"),
                             "All ages")
  expect_equal(expected, TRUE)
})

test_that("compare_totals gives FALSE when totals dont match - registrations", {

  dummy_data_df <- data.frame(
    week_number = rep(1, 6),
    age = c(rep("All ages", 4), rep("0 to 29", 4), rep("30 to 44", 4)),
    area_of_usual_residence = rep(c(rep("England, Wales and non-residents", 2),
                                    rep("England", 2)), 3),
    sex = rep(c("Female", "Male"), 6),
    number_of_deaths = c(10, 12, 7, 8, 6, 7, 5, 7, 4, 5, 2, 3)
  )
  expected <- compare_totals(dummy_data_df, "registrations", "age",
                             c("0 to 29", "30 to 44"),
                             c("sex", "area_of_usual_residence"),
                             "All ages")
  expect_equal(expected, FALSE)
})

test_that("compare_totals gives FALSE when totals dont match - occurrences", {

  dummy_df_2024 <- data.frame(
    year = rep(2024, 6),
    week_number = rep(1, 6),
    age = c(rep("All ages", 4), rep("0 to 29", 4), rep("30 to 44", 4)),
    area_of_usual_residence = rep(c(rep("England, Wales and non-residents", 2),
                                    rep("England", 2)), 3),
    sex = rep(c("Female", "Male"), 6),
    number_of_deaths = c(10, 12, 7, 8, 6, 7, 5, 7, 4, 5, 2, 3)
  )

  dummy_df_2023 <- data.frame(
    year = rep(2023, 6),
    week_number = rep(1, 6),
    age = c(rep("All ages", 4), rep("0 to 29", 4), rep("30 to 44", 4)),
    area_of_usual_residence = rep(c(rep("England, Wales and non-residents", 2),
                                    rep("England", 2)), 3),
    sex = rep(c("Female", "Male"), 6),
    number_of_deaths = c(10, 12, 7, 8, 6, 7, 5, 7, 4, 5, 2, 3)
  )

  dummy_data_comb <- rbind(dummy_df_2024, dummy_df_2023)

  expected <- compare_totals(dummy_data_comb, "occurrences", "age",
                             c("0 to 29", "30 to 44"),
                             c("sex", "area_of_usual_residence"),
                             "All ages")
  expect_equal(expected, FALSE)
})

test_that("due_involve_check on df values when due > involve", {

  dummy_data_df <- data.frame(
    week_number = rep(1, 4),
    week_ending = rep("8 March 2024", 4),
    area_of_usual_residence =
      c(rep("Wales", 2), rep("England, Wales and non-residents", 2)),
    cause_of_death = rep(
      c("Deaths due to COVID-19 (U07.1, U07.2, U10.9)",
        "Deaths involving to COVID-19 (U07.1, U07.2, U09.9, U10.9)"), 2),
    number_of_deaths = c(5, 4, 6, 7)
  )

  expected <- data.frame(
    `Week number` = 1,
    `Week ending` = "8 March 2024",
    `Area of usual residence` = "Wales",
    `Cause of death` = "U07.1, U07.2, U09.9, U10.9",
    `Deaths due to` = 5,
    `Deaths involving` = 4,
    check.names = FALSE
  )
  df_actual <- data.frame(due_involve_check(dummy_data_df), check.names = FALSE)
  expect_equal(df_actual, expected)
})

test_that("Checks tables outputs for differences correctly",
          {
  df_base <- data.frame(
    "Week number" = c(5, 4, 3, 2, 1, 5, 4, 3, 2, 1, 5, 4, 3, 2, 1, 5, 4, 3,
                      2, 1),
    "Area of usual residence" = c(rep("England, Wales and Non-residents", 5),
                                  rep("England", 5),
                                  rep("Wales", 5),
                                  rep("Boop", 5)),
    "Number of deaths" = c(2200, 1900, 1899, 5500, 6500,
                           1999, 2999, 2000, 200, 2000,
                           299, 322, 233, 244, 244,
                           1, 2, 3, 4, 5),
    check.names = FALSE
  )

  df_check <- data.frame(
    "Week number" = c(5, 4, 3, 2, 1, 5, 4, 3, 2, 1, 5, 4, 3, 2, 1),
    "Area of usual residence" = c(rep("England, Wales and Non-Residents", 5),
                                  rep("England", 5),
                                  rep("Wales", 5)),
    "Number of deaths" = c(2200, 1900, 1899, 5500, 6500,
                           1999, 2999, 2000, 200, 2000,
                           299, 322, 233, 244, 244),
    check.names = FALSE
  )

  actual <- check_area_outputs_match(df_base,
                                     df_check,
                                     c("England, Wales and Non-Residents",
                                       "England", "Wales"),
                                     "Area of usual residence",
                                     "Number of deaths")

  expected <- data.frame(
    `Area Level Checked` = c("England, Wales and Non-Residents", "England",
                             "Wales", "Other areas"),
    match = c(TRUE, TRUE, TRUE, "Numeric: lengths (20, 15) differ"),
    check.names = FALSE
  )

  expect_equal(actual, expected)
})

test_that("Check tables outputs are matching", {
  df_base <- data.frame(
    "Week number" = c(5, 4, 3, 2, 1, 5, 4, 3, 2, 1, 5, 4, 3, 2, 1),
    "Area of usual residence" = c(rep("England, Wales and Non-residents", 5),
                                  rep("England", 5),
                                  rep("Wales", 5)),
    "Number of deaths" = c(2200, 1900, 1899, 5500, 6500,
                           1999, 2999, 2000, 200, 2000,
                           299, 322, 233, 244, 244),
    check.names = FALSE
  )

  df_check <- data.frame(
    "Week number" = c(5, 4, 3, 2, 1, 5, 4, 3, 2, 1, 5, 4, 3, 2, 1),
    "Area of usual residence" = c(rep("England, Wales and Non-Residents", 5),
                                  rep("England", 5),
                                  rep("Wales", 5)),
    "Number of deaths" = c(2200, 1900, 1899, 5500, 6500,
                           1999, 2999, 2000, 200, 2000,
                           299, 322, 233, 244, 244),
    check.names = FALSE
  )

  actual <- check_area_outputs_match(df_base,
                                     df_check,
                                     c("England, Wales and Non-Residents",
                                       "England", "Wales"),
                                     "Area of usual residence",
                                     "Number of deaths")

  expected <- data.frame(
    `Area Level Checked` = c("England, Wales and Non-Residents", "England",
                             "Wales", "Other areas"),
    match = c(TRUE, TRUE, TRUE, TRUE),
    check.names = FALSE
  )

  expect_equal(actual, expected)
})


test_that("get_rows_asmr_outside_limits shows when values outside", {

  dummy_data_df <- data.frame(
    `Week number` = c(1, 2, 3),
    `Country` = c("Wales", "England", "Scotland"),
    `Sex` = c("Male", "Female", "All people"),
    `ASMR per 100,000` = c(400, 500, 600),
    `Lower confidence limit` = c(500, 450, 550),
    `Upper confidence limit` = c(600, 490, 650),
    check.names = FALSE
  )

  expected <- data.frame(
    `Week number` = c(1, 2),
    `Country` = c("Wales", "England"),
    `Sex` = c("Male", "Female"),
    `ASMR per 100,000` = c(400, 500),
    `Lower confidence limit` = c(500, 450),
    `Upper confidence limit` = c(600, 490),
    check.names = FALSE
  )

  df_actual <- get_rows_asmr_outside_limits(dummy_data_df)
  expect_equal(df_actual, expected)
})

test_that("get_rows_asmr_outside_limits empty df when none fail", {

  dummy_data_df <- data.frame(
    `Week number` = c(1, 2, 3),
    `Country` = c("Wales", "England", "Scotland"),
    `Sex` = c("Male", "Female", "All people"),
    `ASMR per 100,000` = c(400, 500, 600),
    `Lower confidence limit` = c(300, 450, 550),
    `Upper confidence limit` = c(600, 510, 650),
    check.names = FALSE
  )

  expected <- data.frame(
    `Week number` = integer(),
    `Country` = character(),
    `Sex` = character(),
    `ASMR per 100,000` = double(),
    `Lower confidence limit` = double(),
    `Upper confidence limit` = double(),
    check.names = FALSE
  )

  df_actual <- get_rows_asmr_outside_limits(dummy_data_df)
  expect_equal(df_actual, expected)
})

test_that("get_rows_uk_asmr_out_ctrys shows when values outside", {

  dummy_data_df <- data.frame(
    `Week number` = c(1, 1, 1),
    `Country` = c("UK", "England", "Scotland"),
    `Sex` = c("Male", "Male", "Male"),
    `ASMR per 100,000` = c(400, 500, 600),
    `Lower confidence limit` = c(500, 450, 550),
    `Upper confidence limit` = c(600, 490, 650),
    check.names = FALSE
  )

  expected <- data.frame(
    `Week number` = c(1),
    `Country` = c("UK"),
    `Sex` = c("Male"),
    `ASMR per 100,000` = c(400),
    `Lower confidence limit` = c(500),
    `Upper confidence limit` = c(600),
    min_ctry_asmr = c(500),
    max_ctry_asmr = c(600),
    check.names = FALSE
  )

  df_actual <- get_rows_uk_asmr_out_ctrys(dummy_data_df)
  expect_equal(df_actual, expected)
})

test_that("get_rows_asmr_all_ppl_out_m_f shows when values outside", {

  dummy_data_df <- data.frame(
    `Week number` = c(1, 1, 1, 2, 2, 2),
    `Country` = rep("England", 6),
    `Sex` = c("All people", "Male", "Female"),
    `ASMR per 100,000` = c(400, 500, 600, 700, 690, 710),
    `Lower confidence limit` = c(500, 450, 550, 650, 640, 760),
    `Upper confidence limit` = c(600, 490, 650, 800, 790, 900),
    check.names = FALSE
  )

  expected <- data.frame(
    `Week number` = c(1),
    `Country` = c("England"),
    `All people` = c(400),
    Male = c(500),
    Female = c(600),
    check.names = FALSE
  )
  class(expected) <- c("tbl_df", "tbl", "data.frame")

  df_actual <- get_rows_asmr_all_ppl_out_m_f(dummy_data_df)
  expect_equal(df_actual, expected)
})
