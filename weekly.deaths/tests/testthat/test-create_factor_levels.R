test_that("Factor levels object created correctly", {

  expected <- list(
    agegrp_5yr = c(
      "All ages", "Under 1", "1 to 4", "5 to 9", "10 to 14", "15 to 19",
      "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
      "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79",
      "80 to 84", "85 to 89", "90 to 94", "95 to 99", "100 and over"),
    agegrp_excess = c(
      "All ages", "0 to 29", "30 to 44", "45 to 49", "50 to 54", "55 to 59",
      "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89",
      "90 and over"),
    agegrp_wide = c(
      "All ages", "Under 1", "1 to 14", "15 to 44", "45 to 64", "65 to 74",
      "75 to 84", "85 and over"),
    sex = c("All people", "Female", "Male"),
    region = c(
      "North East", "North West", "Yorkshire and The Humber", "East Midlands",
      "West Midlands", "East of England", "London", "South East", "South West"),
    country = c(
      "UK", "England, Wales and non-residents", "England", "Wales", "Scotland",
      "Northern Ireland"),
    place_of_death = c(
      "All places", "Home", "Hospital", "Hospice", "Care home", "Elsewhere"),
    causes = c(
      "All causes",
      "Deaths involving diseases of the respiratory system (J00 to J99)",
      "Deaths due to diseases of the respiratory system (J00 to J99)",
      "Deaths involving influenza or pneumonia (J09 to J18)",
      "Deaths due to influenza or pneumonia (J09 to J18)",
      "Deaths involving COVID-19 (U07.1, U07.2, U09.9, U10.9)",
      "Deaths due to COVID-19 (U07.1, U07.2, U10.9)"),
    country_region = c(
      "England, Wales and non-residents", "England", "Wales", "North East",
      "North West", "Yorkshire and The Humber", "East Midlands",
      "West Midlands", "East of England", "London", "South East", "South West"),
    place_of_death_other = c(
      "All places", "Home", "Hospital", "Care home", "Other"),
    cert_type = c(
      "All deaths", "Certified by doctor", "Certified by coroner",
      "Certified by coroner with inquest",
      "Certified by coroner with no inquest", "Certified by medical examiner",
      "Unknown/Uncertified"),
    imd = c("All groups", paste0("Decile ", 1:10), paste0("Quintile ", 1:5))
  )

  expect_equal(expected, create_factor_levels())
})
