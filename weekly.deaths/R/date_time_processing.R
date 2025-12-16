#' Derive week and year information for dates of death occurrences or
#' registration
#'
#' @description This function takes a dataframe and calculates the start date
#' of the year and the publication year for each date. It then calculates the
#' week number based on the derived year start date. This is done for either
#' `dor` (registration) or `dod` (occurrences).
#' Note: The week and year number derivation is based on a modified version of
#' the ISO 8601 standard. In this modification, the year's start date is
#' considered to be the Saturday preceding the first week of the year. The
#' first week of the year is defined as the week containing the first Thursday
#' of the year. Week end number is also added.
#'
#' @param data Dataframe with the columns: `dor` and `dod`, which
#' contain dates in YYYYMMDD format and are integer type.
#' @param current_year analytical year
#' @param col String of either "dor" or "dod", default is "dor".
#' @param is_scot logical, default is FALSE
#'
#' @return The input dataframe with 3 new columns: year, week (week number) and
#' week_ending, for either registration or occurrence date.
#'
#' @export
#'
derive_years_week_numbers <- function(data,
                                      current_year,
                                      col = "dor",
                                      is_scot = FALSE) {
  # Calculating the publication year and week for each date in data
  data %>%
    calc_years_start_dates(col, is_scot) %>%
    calc_week_number_week_ending(col, current_year)
}

#' Calculate year start dates and their corresponding years for deaths data
#'
#' @description This function calculates the start date of the year for each
#' date in a given vector, and associates these dates with their corresponding
#' years. Gives a warning if `col` is "dor" and there are any 'missing' years
#' between max and min where there are no deaths.
#'
#' @details Use a range from one year before to one year after to catch edge
#' cases of missing calendar years where there are no registrations and  weeks
#' at the start of the first calendar year that are in the previous registration
#' year, or at end of the last calendar year that are in the next
#' registration year.
#'
#' @param data Deaths dataframe.
#' @param col String of date column name to use to create new columns from:
#' either "dor" or "dod".
#' @param is_scot logical, default is FALSE
#'
#' @return Deaths dataframe with extra column for year_start_date and year of
#' chosen column.
#'
#' @export
#'
calc_years_start_dates <- function(data, col, is_scot = FALSE) {
  # Years present in data needs to be sorted due to use of cut
  years_in_data <- data[[col]] %>%
    lubridate::ymd() %>%
    lubridate::year() %>%
    unique() %>%
    sort()

  if (col == "dor" &&
      length(years_in_data) != length(min(years_in_data):max(years_in_data))) {
    warning(paste("There are calendar years in 'dor' with no data.",
                  "Please investigate."))
  }

  # Adding the year before the first year of data in case there are occur from
  # the prev publishing year/week, and the year after in case there is data for
  # a calendar year which should be in the next registration year, e.g. 20221231
  # when there is no 2023 data.
  years_range <- (min(years_in_data) - 1):(max(years_in_data) + 1)

  year_start_dates <- data.frame(
    year = years_range,
    start_date = sapply(years_range,
                        calc_start_date_for_year,
                        is_scot = is_scot))

  data %>%
    dplyr::mutate(
      # This is a temp column to calc pub week number
      year_start_date = as.numeric(
        as.character(
          cut(
            .data[[col]],
            breaks = c(year_start_dates$start_date, Inf),
            right = FALSE,
            labels = year_start_dates$start_date
            )
          )
        ),
      year = as.numeric(
        as.character(
          cut(
            .data[[col]],
            breaks = c(year_start_dates$start_date, Inf),
            right = FALSE,
            labels = year_start_dates$year
            )
          )
        )
      )
}

#' Calculates the start date for a year
#'
#' @description Uses the ISO format to calculate the start of the year for this
#' analysis from the defined week 1 (W01-1). As we want the Thursday before the
#' starting Saturday of week 1, we take 2 away from the ISO week 1 date (2 days
#' before Saturday is Thursday).
#'
#' @details Format of ISO week is "YYYY-W__-num", where:
#' * W__ is a range of W01 to W52 or W53 (depending on the year, there may be 53
#' defined weeks in a year, if there is a leap year).
#' * num refers to the day of the ISO week, range 1 to 7, where 1 is Saturday.
#' e.g. 2023-W01-1 means the first day of the ISO week 1 in 2023.
#' Scotland uses different week start day, so the processing is different.
#'
#' @param year Integer year in YYYY format.
#' @param is_scot logical, default is FALSE
#'
#' @return Numeric date of start date to use for the year in format YYYYMMDD
#' @export
#'
calc_start_date_for_year <- function(year, is_scot = FALSE) {

  if (is_scot == FALSE) {
    start_date <- ISOweek::ISOweek2date(paste(year, "W01-1", sep = "-")) - 2
  } else if (is_scot == TRUE) {
    start_date <- ISOweek::ISOweek2date(paste(year, "W01-1", sep = "-"))
  }
  as.numeric(format(start_date, "%Y%m%d"))
}

#' Calculate week number and week end date
#'
#' @description This function calculates the week number and week end date for a
#' given set of dates. The week number is based on the difference between each
#' date and a corresponding start date. It is calculated as the ceiling of the
#' number of weeks since the start date. Week end date is 6 days after the start
#' of that week (so difference in weeks minus 1 day).
#'
#' @details When there is an existing column of `dor_week` (as in EW data), a
#' check will be done to compare the existing value of `dor_week` and the
#' created `week`. A warning will be given if there is a mismatch.
#'
#' @param data Deaths dataframe with year start date and year calculated (output
#' of calc_years_start_dates).
#' @param col String of date column name to use to create new columns from:
#' either "dor" or "dod".
#' @param current_year analytical year
#'
#' @return Deaths dataframe with extra columns for week number and week ending.
#' `year_start_date` column is dropped.
#'
#' @export
#'
calc_week_number_week_ending <- function(data, col, current_year) {

  data <- data %>%
    dplyr::mutate(
      week =
        as.numeric(ceiling(
          (lubridate::ymd(.data[[col]]) -
             (lubridate::ymd(year_start_date) - 1)) / 7))) %>%
    dplyr::mutate(
      week_ending =
        stringr::str_remove(
          format(
            (lubridate::ymd(year_start_date) +
               lubridate::weeks(week) - 1), "%d %B %Y"), "^0+")) %>%
    dplyr::select(-year_start_date)

  if ("dor_week" %in% colnames(data) && col == "dor") {
    test_dor_week_no <- dplyr::mutate(
      data,
      diff = ifelse(
        dor_week != week & year == current_year,
        TRUE, FALSE))
    if (sum(test_dor_week_no$diff, na.rm = TRUE) > 0) {
      warning(paste("The original data dor_week does not match the Week number",
                    "column created. Please investigate."))
    }
  }

  data
}

#' Determine what the maximum date should be for considered registration dates.
#'
#' @param cfg Config as named list containing year and week named elements.
#' year as integer in format YYYY and week as string.
#' @param is_scot logical, default is FALSE
#'
#' @return character containing the max date in the format YYYYMMDD
#' @export
#'
derive_max_reg_date <- function(cfg, is_scot = FALSE) {
  format(lubridate::ymd(calc_start_date_for_year(cfg$year, is_scot)) +
           lubridate::weeks(as.numeric(cfg$week)) - 1,
         "%Y%m%d")
}

#' Drop records with registration date before the start of the analysis year
#'
#' @param data Deaths dataframe with `dor` column.
#' @param current_year analytical year as numeric e.g. 2024
#' @param is_scot logical, default is FALSE
#'
#' @return Deaths dataframe with records before the start of the analysis year.
#'
#' @export
#'
filter_before_year_start <- function(data, current_year, is_scot = FALSE) {
  dplyr::filter(
    data,
    dor >= calc_start_date_for_year(current_year, is_scot))
}

#' Calculates end date for a year
#'
#' @param year year as numeric, e.g. 2024
#' @param is_scot is_scot logical, default is FALSE; for Scotland it is
#' standard ISO week
#'
#' @return date for end of the year as numeric, in format YYYYMMDD
#' @export
#'
calc_year_end_date <- function(year, is_scot = FALSE) {
  start_date <- as.character(
    calc_start_date_for_year(year + 1, is_scot = is_scot))
  end_date <- as.Date(start_date, format = "%Y%m%d")  - 1
  as.numeric(format(end_date, "%Y%m%d"))
}

#' Calculate numbers of weeks in a year
#'
#' @param year year as numeric, e.g. 2024
#'
#' @return number of weeks in the year (52 or 53), as numeric
#' @export
#'
calc_weeks_in_year <- function(year) {
  end_date <- as.Date(as.character(calc_year_end_date(year, TRUE)), "%Y%m%d")
  lubridate::isoweek(end_date)
}


#' Calculates the date one week before a given date
#'
#' @details The date needs to be a string in format yyyymmdd. The returned
#' string will be in the same format
#'
#' @param date_as_string string of 8 digits e.g. "20240131"
#'
#' @return a string of the date one week before the input string
#' @export
#'
calc_date_one_week_before <- function(date_as_string) {
  date <- as.Date(date_as_string, "%Y%m%d")
  date_one_week_prior <- date  - lubridate::days(7)
  format(date_one_week_prior, "%Y%m%d")
}
