test_that("Output of ew_deaths_for_5y_averages", {

  data_finalised <- data.frame(
    dor = c(20181201, 20191102, 20210101, 20220504),
    ctryr = c(921, 921, 924, 900),
    country = c("England", "England", "Wales", "Nonresident"),
    week = c(47, 42, 1, 22),
    year = c(2018, 2019, 2021, 2022),
    other = 1:4
  )

  data_provisional <- data.frame(
    dor  = c(20230105, 20230201, 20240102),
    country = c("England", "England", "Wales"),
    other = 1:3,
    week = c(1, 5, 1),
    year = c(2023, 2023, 204),
    dod = c(20221201, 20230201, 20231115),
    agegrp_5yr = c("45 to 64", "65 to 74", "75 to 84"),
    dor_week  = c(47, 1, 44),
    region = c("London", "South East", "North West")
  )

  years_provisional <- c(2023)

  expected <- data.frame(
    dor = c(20181201, 20191102, 20210101, 20220504, 20230105, 20230201),
    country = c("England", "England", "Wales", "Nonresident", "England",
                "England"),
    week = c(47, 42, 1, 22, 1, 5),
    year = c(2018, 2019, 2021, 2022, 2023, 2023),
    other = c(1:4, 1:2)
  )

  expect_equal(process_5y_average_deaths(data_finalised,
                                         data_provisional,
                                         years_provisional),
               expected)
})

test_that("Output of ew_deaths_for_5y_averages when all data is finalised", {

  data_finalised <- data.frame(
    dor = c(20181201, 20191102, 20210101, 20220504, 20230303),
    ctryr = c(921, 921, 924, 900, 924),
    country = c("England", "England", "Wales", "Nonresident", "Wales"),
    week = c(47, 42, 1, 22, 11),
    year = c(2018, 2019, 2021, 2022, 2023),
    other = 1:5
  )

  data_provisional <- data.frame(
    dor  = c(20230105, 20230201, 20240102),
    country = c("England", "England", "Wales"),
    other = 1:3,
    week = c(1, 5, 1),
    year = c(2023, 2023, 204),
    dod = c(20221201, 20230201, 20231115),
    agegrp_5yr = c("45 to 64", "65 to 74", "75 to 84"),
    dor_week  = c(47, 1, 44),
    region = c("London", "South East", "North West")
  )

  years_provisional <- c()

  expected <- data.frame(
    dor = c(20181201, 20191102, 20210101, 20220504, 20230303),
    country = c("England", "England", "Wales", "Nonresident", "Wales"),
    week = c(47, 42, 1, 22, 11),
    year = c(2018, 2019, 2021, 2022, 2023),
    other = c(1:5)
  )

  expect_equal(process_5y_average_deaths(data_finalised,
                                         data_provisional,
                                         years_provisional), expected)
})

test_that("Five year average calculated correctly", {
  df <- data.frame(
    year = c(rep(1, 8), rep(2, 4), rep(3, 2), rep(4, 2), rep(5, 2), 1, 2, 3),
    week = c(rep(1, 18), rep(2, 3)),
    country = c(rep(c("e", "e", "w", "w"), 5), "w"),
    place_of_death = c(rep(c("a", "b"), 9), rep("a", 3)),
    other = 1:21
  )

  expected <- data.frame(
    week = c(1, 2, 1, 1, 2),
    area = c(rep("England, Wales and non-residents", 2), "e", "w", "w"),
    avg_5y = c(3.6, 0.6, 2.0, 1.6, 0.6)
  )

  actual <- average_5y(df)
  expect_equal(actual, expected)
})

test_that("Five year average calculated correctly with groupings", {
  df <- data.frame(
    year = c(rep(1, 8), rep(2, 4), rep(3, 2), rep(4, 2), rep(5, 2), 1, 2, 3),
    week = c(rep(1, 18), rep(2, 3)),
    country = c(rep(c("e", "e", "w", "w"), 5), "w"),
    place_of_death = c(rep(c("a", "b"), 9), rep("a", 3)),
    other = 1:21
  )

  expected <- data.frame(
    week = c(1, 1, 2, 1, 1, 1, 1, 2),
    area = c(rep("England, Wales and non-residents", 3), "e", "e", rep("w", 3)),
    place_of_death = c("a", "b", "a", "a", "b", "a", "b", "a"),
    avg_5y = c(1.8, 1.8, 0.6, 1, 1, 0.8, 0.8, 0.6)
  )

  actual <- average_5y(df, "place_of_death")
  expect_equal(actual, expected)
})

test_that("Percentage change calculated and rounded", {
  expect_equal(calc_p_change(1.2222, 1), 22.2)
})

test_that("Percentage change calculated and rounded", {
  expect_equal(calc_p_change(1.2225, 1), 22.3)
})
