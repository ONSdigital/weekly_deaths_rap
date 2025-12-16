#' Create final occurrences table by combining current and previous occurrences
#' data
#'
#' @param df1 data frame of deaths in England and Wales filtered to current year
#' @param df2 data frame of deaths in England and Wales not filtered
#' @param imd list containing items england and wales, the imd lookups
#' @param config list which includes items year (current year - numeric e.g.
#' 2024), week (analysis week - numeric e.g. 24), previous_year (previous_year,
#' numeric e.g. 2023)
#'
#' @return A data-frame combining two years of summarised death counts.
#' @export
#'
create_ew_occurrences_table <- function(df1,
                                 df2,
                                 imd,
                                 config) {

  current_year <- config$year
  current_week <- config$week
  previous_year <- config$previous_year

  df1 <- derive_years_week_numbers(df1, current_year, "dod")
  df2 <- derive_years_week_numbers(df2, current_year, "dod")

  ew_occurrences_current <- df1 %>%
    count_dths_geo_age_sex_pod(current_year, current_week) %>%
    bind_imd_values(df1, current_week, current_year, imd) %>%
    format_dths_geog_age_sex_pod_imd() %>%
    dplyr::mutate(Year = as.numeric(current_year))

  weeks_in_previous_year <- calc_weeks_in_year(as.numeric(previous_year))

  ew_occurrences_past <- df2 %>%
    count_dths_geo_age_sex_pod(previous_year, weeks_in_previous_year) %>%
    bind_imd_values(
      df2, weeks_in_previous_year, previous_year, imd) %>%
    format_dths_geog_age_sex_pod_imd() %>%
    dplyr::mutate(Year = as.numeric(previous_year))

  ew_occurrences_current %>%
    rbind(ew_occurrences_past) %>%
    dplyr::relocate(Year, .before = "Week number") %>%
    check_columns_present(
      c("Year", "Week number", "Week ending", "Area of usual residence", "Sex",
        "Age group (years)", "IMD quantile group", "Place of occurrence",
        "Number of deaths"))
}
