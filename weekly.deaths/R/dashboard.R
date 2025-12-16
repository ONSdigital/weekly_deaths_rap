#' Creates table for evergreen dashboard
#' @description Creates table for evergreen dashboard, calling 3 sub-functions
#' to bring in England and Wales deaths (by country and cause), expected_deaths
#' and UK deaths. If there are more than 2 calendar years in the data,
#' rows are dropped to result in only a specific number of years of data.
#'
#' @param ew_deaths Processed EW weekly deaths data frame, containing columns
#' region, cause, dor_year, dor_week, and week_ending.
#' @param exp_deaths_sql Data frame containing expected death data including
#' area, age, sex.
#' @param scot_df Processed Scotland dataframe.
#' @param ni_df Processed Northern Ireland dataframe.
#' @param cfg Config containing `year`, `week`, `scope` and `chart_years`.
#' @param num_yr_to_keep Numeric for how many years of data to keep.
#'
#' @return Data frame with columns for England and Wales deaths (by country
#' and cause), expected_deaths and UK deaths.
#'
#' @export
#'
create_dashboard_data <- function(
    ew_deaths, exp_deaths_sql, scot_deaths, ni_deaths, cfg, num_yr_to_keep) {

  dashboard <- dplyr::full_join(
    calc_dths_cause_dashboard(ew_deaths),
    dplyr::full_join(
      calc_exp_deaths_dashboard(exp_deaths_sql, ew_deaths, cfg),
      calc_uk_deaths_dashboard(ew_deaths, scot_deaths, ni_deaths, cfg),
      by = c("dor_year" = "year", "dor_week" = "week")),
    by = c("week_ending")) %>%
    dplyr::rename(year = dor_year)

  if (length(unique(dashboard$year)) > num_yr_to_keep) {
    dashboard <- dplyr::filter(
      dashboard,
      !((year < (max(cfg$chart_years) - num_yr_to_keep)) |
          (year == (max(cfg$chart_years) - num_yr_to_keep) & week < cfg$week)))
  }
  if (cfg$scope == "EW") {
    print("The dashboard dataset does not contain UK data for the latest week.")
    print("Run the pipeline with cfg$scope <- 'UK' for the dashboard data.")
  }
  dashboard
}


#' Creates table of deaths by cause for evergreen dashboard
#' @description Modifies existing deaths data frame "ew_deaths"
#' to evergreen dashboard specifications. Filters and pivots to create columns
#' for registered deaths, by country and cause.
#'
#' @param ew_deaths Processed EW weekly deaths data frame, containing columns
#' region, cause, dor_year, dor_week, and week_ending.
#'
#' @return Data frame with columns registered deaths, by country and cause.
#'
#' @export
#'
calc_dths_cause_dashboard <- function(ew_deaths) {

  ew_deaths %>%
    count_dths_cause_geo() %>%
    dplyr::filter(
      region %in% c("England, Wales and non-residents", "England", "Wales")) %>%
    tidyr::pivot_wider(
      names_from = c(region, cause), values_from = n, names_sep = " ")
}


#' Creates table of expected deaths for evergreen dashboard
#' @description Modifies existing expected deaths data frame "exp_deaths_sql"
#' to evergreen dashboard specifications. Filters, groups and pivots to create
#' columns for expected deaths by country.
#'
#' @param exp_deaths_sql Data frame containing expected death data including
#' area, age, sex.
#' @param ew_deaths Processed EW weekly deaths data frame, containing columns
#' region, cause, dor_year, dor_week, and week_ending.
#' @param cfg Config containing `year` and `week`
#'
#' @return Data frame with columns expected_deaths.
#'
#' @export
#'
calc_exp_deaths_dashboard <- function(exp_deaths_sql, ew_deaths, cfg) {

  ew_deaths <- ew_deaths %>%
    dplyr::select(year, week, week_ending) %>%
    dplyr::distinct()

  exp_deaths_sql %>%
    dplyr::filter(
      area %in% c("England, Wales and non-residents", "England", "Wales"),
      sex == "All people",
      esp_age_group == "All ages",
      !(dor_year == cfg$year & dor_week > cfg$week)) %>%
    dplyr::select(area, dor_year, dor_week, expected_deaths) %>%
    tidyr::pivot_wider(names_from = area,
                       values_from = expected_deaths,
                       names_prefix = "Expected deaths ") %>%
    dplyr::left_join(
      ew_deaths,
      by = c("dor_year" = "year", "dor_week" = "week"))
}


#' Creates table of UK deaths for evergreen dashboard
#' @description Uses existing function "create_uk_dths_ctry_age_sex" to create
#' UK deaths data frame and modifies to evergreen dashboard specifications.
#' Filters, groups and pivots to create columns for UK deaths.
#'
#' @param ew_deaths Processed EW weekly deaths data frame, containing columns
#' region, cause, dor_year, dor_week, and week_ending.
#' @param scot_df Processed Scotland dataframe.
#' @param ni_df Processed Northern Ireland dataframe.
#' @param cfg Config containing `year`, `week`, `scope` and `chart_years`.
#'
#' @return Data frame with UK death column.
#'
#' @export
#'
calc_uk_deaths_dashboard <- function(ew_deaths, scot_deaths, ni_deaths, cfg) {

  ew_deaths %>%
    create_uk_dths_ctry_age_sex(
      scot_df = scot_deaths,
      ni_df = ni_deaths,
      current_year = min(cfg$chart_years),
      current_week = cfg$week,
      scope = cfg$scope,
      age_group = "agegrp_wide"
      ) %>%
    dplyr::filter(
      country == "UK",
      sex == "All people",
      agegrp_wide == "All ages",
      !(year == cfg$year & week > cfg$week)
      ) %>%
    dplyr::select(country, year, week, deaths) %>%
    tidyr::pivot_wider(names_from = country, values_from = deaths)
}


#' Format data ready for Evergreen dashboard
#'
#' @description Creates a publication date column (date type) using the config
#' variable in the format "YYYY-MM-DD". Any column starting with
#' "Expected deaths" is rounded to specified number of decimal places.
#' The `week_ending` column is also converted into format "YYYY-MM-DD".
#' The data frame is sorted by the `week_ending` and only required columns
#' are selected and named as appropriate.
#'
#' @details The format of the output dataframe should be agreed with publishing
#' as it is used in their pipeline to produce the Evergreen dashboard. No
#' changes without communication.
#'
#' @param data Data frame containing deaths registered, expected deaths and
#' deaths by cause (both due and involving COVID-19 and influenza or pneumonia)
#' for England and Wales.
#' @param cfg Config containing `publication_date`.
#' @param round_to Integer used to round expected death to this decimal place.
#'
#' @return Formatted data frame containing deaths registered, expected deaths
#' and deaths by cause. Order columns and rows to dashboard specification.
#'
#' @export
#'
format_dashboard <- function(data, cfg, round_to = 0) {

  data %>%
    dplyr::mutate(
      publication_date = as.Date(lubridate::dmy(cfg$publication_date),
                            format = "%Y-%m-%d"),
      dplyr::across(dplyr::starts_with("Expected deaths "),
                    ~ round(.x, digits = round_to)),
      week_ending = as.Date(lubridate::dmy(week_ending),
                            format = "%Y-%m-%d")) %>%
    dplyr::arrange(week_ending) %>%
    dplyr::select(
      publication_date,
      year,
      week_num = dor_week,
      date = week_ending,
      num_uk = UK,
      num_england_wales = `England, Wales and non-residents All causes`,
      exp_england_wales = `Expected deaths England, Wales and non-residents`,
      num_england = `England All causes`,
      num_wales = `Wales All causes`,
      exp_england = `Expected deaths England`,
      exp_wales = `Expected deaths Wales`,
      covid_due_to = paste("England, Wales and non-residents Deaths due to",
                           "COVID-19 (U07.1, U07.2, U10.9)"),
      covid_with = paste("England, Wales and non-residents Deaths involving",
                         "COVID-19 (U07.1, U07.2, U09.9, U10.9)"),
      flu_due_to = paste("England, Wales and non-residents Deaths due to",
                         "influenza or pneumonia (J09 to J18)"),
      flu_with = paste("England, Wales and non-residents Deaths involving",
                       "influenza or pneumonia (J09 to J18)"))
}
