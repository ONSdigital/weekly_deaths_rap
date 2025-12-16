#' Registration delays by week of registration and death certification type
#' (England and Wales)
#'
#' @description Uses England and Wales death registration data to create
#' number of deaths registered within 7 days, percentage of deaths registered
#' within 7 days, median delay and IQR of delay by certification type.
#'
#' @details Counts certified by coroner are double counted between the
#' aggregation group and the with/without inquest so the total counts do not
#' equal 100%.
#'
#' @param data Death registration data.
#'
#' @return A dataframe of delay data for all weeks by certification type.
#' @export
#'
create_delay_summary_certtype <- function(data) {

  dplyr::bind_rows(
    create_delay_summary(data, c("week", "week_ending")),
    create_delay_summary(data, c("week", "week_ending", "cert_type")),
    create_delay_summary(data, c("week", "week_ending", "cert_type_agg")) %>%
      dplyr::filter(cert_type_agg %in% "Certified by coroner")) %>%
    dplyr::mutate(
      cert_type = dplyr::coalesce(cert_type, cert_type_agg),
      cert_type = dplyr::if_else(is.na(cert_type), "All deaths",
                                 as.character(cert_type))) %>%
    dplyr::select(-cert_type_agg)

}

#' Registration delays by week of registration, selected death certification
#' type, selected area and place of death.
#'
#' @description Uses England and Wales death registration data to create
#' number of deaths registered within 7 days, percentage of deaths registered
#' within 7 days, median delay and IQR of delay by certification type.
#'
#' @details Counts certified by coroner are double counted between the
#' aggregation group and the with/without inquest so the total counts do not
#' equal 100%.
#' The cert_col uses "cert_type_agg" to calculate the "Certified by coroner"
#' category
#'
#' @param data Death registration data.
#' @param area_col Quoted name of column that refers to the area (either
#' "region" or "country").
#' @param cert_col Quoted name of column that refers to the certification type
#' (either "cert_type" or "cert_type_agg").
#'
#' @return A dataframe of delay data for all weeks by certification type, area,
#' place of death.
#' @export
#'
create_area_cert_pod_delay <- function(data, area_col, cert_col) {

  if (!(area_col %in% c("region", "country"))) {
    stop('area_col argument must be "region" or "country"')
  }

  if (!(cert_col %in% c("cert_type", "cert_type_agg"))) {
    stop('cert_col argument must be "cert_type" or "cert_type_agg"')
  }

  if (area_col == "region") {
    data <- dplyr::filter(data, !(region %in% c("Wales", "Nonresident")))
  }

  if (cert_col == "cert_type_agg") {
    data <- dplyr::filter(data, cert_type_agg %in% "Certified by coroner")
  }

  weeks <- c("week", "week_ending")

  data <- dplyr::bind_rows(
    create_delay_summary(data, c(weeks, cert_col)),
    create_delay_summary(data, c(weeks, cert_col, "place_of_death")),
    create_delay_summary(data, c(weeks, area_col)),
    create_delay_summary(data, c(weeks, area_col, "place_of_death")),
    create_delay_summary(data, c(weeks, cert_col, area_col)),
    create_delay_summary(data, c(weeks, cert_col, area_col, "place_of_death"))
  ) %>%
    dplyr::rename(cert_type = dplyr::all_of(cert_col),
                  area = dplyr::all_of(area_col))

  if (area_col == "region") {
    data <- dplyr::filter(data, !is.na(area))
  }

  if (cert_col == "cert_type_agg") {
    data <- dplyr::filter(data, !is.na(cert_type))
  }

  data
}

#' Registration delays by week of registration, death certification type, area
#' and place of death.
#'
#' @param data Death registration data.
#'
#' @return Data frame of combinations for delays by week, certification type,
#' area and place of death.
#' @export
#'
create_delay_area_pod_cert_sum <- function(data) {

  weeks <- c("week", "week_ending")

  dplyr::bind_rows(
    create_delay_summary(data, weeks),
    create_delay_summary(data, c(weeks, "place_of_death")),
    create_area_cert_pod_delay(data, "country", "cert_type"),
    create_area_cert_pod_delay(data, "region", "cert_type"),
    create_area_cert_pod_delay(data, "country", "cert_type_agg"),
    create_area_cert_pod_delay(data, "region", "cert_type_agg")
  ) %>%
    dplyr::mutate(
      cert_type = tidyr::replace_na(cert_type, "All deaths"),
      place_of_death = tidyr::replace_na(place_of_death, "All places"),
      area = tidyr::replace_na(area, "England, Wales and non-residents"))

}

#' Registration delays by groups of interest (England and Wales)
#'
#' @description Uses England and Wales death registration data to create
#' number of deaths registered within 7 days, percentage of deaths registered
#' within 7 days, median delay and IQR of delay.
#'
#' @param data Death registration data.
#' @param groupings Vector of strings for column names to group by.
#'
#' @return A dataframe of delay data.
#' @export
#'
create_delay_summary <- function(data, groupings) {

  data %>%
    calc_registration_delays() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groupings))) %>%
    dplyr::summarise(
      total_deaths = dplyr::n(),
      delay_within_7 = sum(delay_days <= 6),
      percentage_within_7 = round(delay_within_7 / total_deaths * 100, 1),
      median_delay = round(median(delay_days), 1),
      q1 = round(unname(quantile(delay_days, 0.25), 1)),
      q3 = round(unname(quantile(delay_days, 0.75), 1))) %>%
    dplyr::ungroup()
}

#' Calculate registration delays for death data
#'
#' @description Converts dor and reg_stat_dor to dates and
#' adds new column of the delay in days, throws an error if registration date
#' is before the occurrence date.
#'
#' @param data Data frame of all year death data.
#'
#' @return Data frame with additional delay column.
#' @export
#'
calc_registration_delays <- function(data) {

  data <- dplyr::mutate(
    data,
    dor = as.Date(as.character(dor), format = "%Y%m%d"),
    dod = as.Date(as.character(dod), format = "%Y%m%d"),
    delay_days = as.numeric(difftime(dor, dod, units = c("days")))
  )

  if (min(data$delay_days) < 0) {
    stop("A death registration date is before death occurence date")
  }

  data
}

#' Format registration delay table
#'
#' @description Adds week end date column after week number,
#' sorts by week number descending and renames columns.
#' @param df Unformatted death registration delay df.
#'
#' @return Formatted dataframe of death registration delay data.
#' @export
#'
format_delay_summary <- function(df) {

  n7 <- paste0("Number of deaths that occurred within previous 7 days (delay",
               " 0 to 6 days)")
  p7 <- paste0("Percentage of deaths that occurred within previous 7 days",
               " (delay 0 to 6 days)")

  df %>%
    dplyr::relocate(week_ending, .after = week) %>%
    dplyr::arrange(desc(week)) %>%
    dplyr::rename(
      "Week number" = week,
      "Week ending" = week_ending,
      "Deaths registered in week" = total_deaths,
      "{n7}" := delay_within_7,
      "{p7}" := percentage_within_7,
      "Median time from death to registration (days)" = median_delay,
      "25th percentile of time from death to registration (days)" = q1,
      "75th percentile of time from death to registration (days)" = q3
    )
}

#' Format registration delay table with certification type included
#'
#' @description Add zero counts, renames, relocates and orders columns.
#' @param df Unformatted death registration delay dataframe.
#'
#' @return Formatted dataframe of death registration delay data by certification
#' type.
#' @export
#'
format_delay_summary_certtype <- function(df) {

  n7 <- paste("Number of deaths that occurred within previous 7 days (delay",
              "0 to 6 days)")
  p7 <- paste("Percentage of deaths that occurred within previous 7 days",
              "(delay 0 to 6 days)")

  df %>%
    dplyr::filter(area != "Nonresident") %>%
    dplyr::mutate(
      cert_type = factor(cert_type, create_factor_levels()$cert_type),
      area = factor(area, create_factor_levels()$country_region),
      place_of_death = factor(
        place_of_death, create_factor_levels()$place_of_death)) %>%
    add_zero_counts_delays() %>%
    dplyr::relocate(week_ending, .after = week) %>%
    dplyr::relocate(area, .after = week_ending) %>%
    dplyr::arrange(desc(week), area, place_of_death, cert_type) %>%
    dplyr::rename(
      "Week number" = week,
      "Week ending" = week_ending,
      "Area of usual residence" = area,
      "Certification type" = cert_type,
      "Place of occurrence" = place_of_death,
      "Deaths registered in week" = total_deaths,
      "{n7}" := delay_within_7,
      "{p7}" := percentage_within_7,
      "Median time from death to registration (days)" = median_delay,
      "25th percentile of time from death to registration (days)" = q1,
      "75th percentile of time from death to registration (days)" = q3
    )
}

#' Add zero counts to delay data frame.
#'
#' @details `cert_type`, `place_of_death` and `area` columns must be factors.
#' Also adds NA to the delay measure cols where there are 0 deaths.
#'
#' @param df Data frame of summary of delays by certification type, place of
#' death and area.
#'
#' @return Data frame with 0 values added where there is no occurrences of the
#' combination.
#' @export
#'
add_zero_counts_delays <- function(df) {

  df %>%
    tidyr::complete(
      tidyr::nesting(week, week_ending), cert_type, place_of_death, area,
      fill = list(total_deaths = 0,
                  delay_within_7 = NA,
                  percentage_within_7 = NA,
                  median_delay = NA,
                  q1 = NA,
                  q3 = NA),
      explicit = FALSE
    )
}
