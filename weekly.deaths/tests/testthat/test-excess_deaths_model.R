
test_that("Excess death age groups created correctly", {

  dummy_data <- data.frame(
    age_group = c("All ages", "Under 1", "1 to 4", "5 to 9", "10 to 14",
                  "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39",
                  "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64",
                  "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
                  "90 and over"),
    other = 1:21
  )

  expected <- data.frame(
    age_group = c("All ages", "Under 1", "1 to 4", "5 to 9", "10 to 14",
                  "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39",
                  "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64",
                  "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
                  "90 and over"),
    other = 1:21,
    agegrp_excess = c("All ages", rep("0 to 29", 7), rep("30 to 44", 3),
                      "45 to 49", "50 to 54", "55 to 59", "60 to 64",
                      "65 to 69", "70 to 74", "75 to 79", "80 to 84",
                      "85 to 89", "90 and over")
  )

  actual <- create_excess_age_groups(dummy_data, age_group)

  expect_equal(actual, expected)
})
