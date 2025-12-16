#' Compute excess deaths using five year average
#'
#' @param weekly_df Provisional deaths dataset with weekly data.
#' @param annual_df Finalised annual deaths dataset containing years of
#' interest.
#' @param years_weekly Numeric vector, specifying which years contained in
#' weekly dataset should be used for computing five year average.
#' @param years_annual Numeric vector, specifying which years contained in
#' annual dataset should be used for computing five year average.
#' @param current_week Analytical week number
#' @param current_year Analytical year as full number, e.g. 2024
#'
#' @return data frame with total deaths for this year, five year average and
#' percentage change from five year average; by week, country and place of death
#' @export
#'
excess_deaths_5ya <- function(weekly_df,
                              annual_df,
                              years_weekly,
                              years_annual,
                              current_week,
                              current_year) {

  weekly_df <- flag_place_of_death_other(weekly_df, "provisional") %>%
    dplyr::filter(week <= as.numeric(current_week))
  annual_df <- process_prev_years(annual_df, years_annual) %>%
    dplyr::filter(week <= as.numeric(current_week))
  data_5y <- process_5y_average_deaths(annual_df, weekly_df, years_weekly)


  dth_5ya <- dplyr::bind_rows(
    average_5y(data_5y),
    average_5y(data_5y, "place_of_death_other")
  )

  weekly_df <- dplyr::filter(weekly_df, year == current_year)
  dth_current <- dplyr::bind_rows(
    group_count(weekly_df, c("week")),
    group_count(weekly_df, c("week", "country")),
    group_count(weekly_df, c("week", "place_of_death_other")),
    group_count(weekly_df, c("week", "country", "place_of_death_other")),
  )

  # put the tables together and format cols
  dths <- dplyr::full_join(dth_current, dth_5ya)
  dths$place_of_death_other[is.na(dths$place_of_death_other)] <- "All places"

  dplyr::mutate(dths, p_change = calc_p_change(deaths, avg_5y))
}

#' Process death data for the 5 year averages
#'
#' @description Filters the provisional death data for the given year(s). Binds
#' the data with the finalised data keeping only the columns that are common to
#' both. Returns a dataframe of the combined datato be used in calculating 5
#' year averages.
#'
#' @details Finalised_years_deaths will already be filtered to required years.
#' Provisional_years_deaths should only contain the current and previous years.
#' `ctryr` columnn to be dropped from finalised years deaths. `dod`,
#' `agegrp_5yr`, `dor_week` and `region` columns to be dropped from
#' prov_years_deaths.
#'
#' @param finalised_years_deaths data frame of finalised years deaths data.
#' @param prov_years_deaths data from of provisional years deaths data.
#' @param prov_years vector of years that provisional data is used.
#'
#' @return data frame of combined finalised and provisional deaths.
#' @export
#'
process_5y_average_deaths <- function(finalised_years_deaths,
                                      prov_years_deaths,
                                      prov_years) {
  finalised_years_deaths <- finalised_years_deaths %>%
    dplyr::select(-ctryr)

  prov_years_deaths <- prov_years_deaths %>%
    dplyr::filter(year %in% prov_years) %>%
    dplyr::select(-c(dod, agegrp_5yr, dor_week, region))

  dplyr::bind_rows(finalised_years_deaths, prov_years_deaths)
}

#' Calculate five year average
#'
#' @param df data frame containing data for five years by year, week, country
#' and desired groupings
#' @param groupings Character vector with additional column names to group by.
#'
#' @return data frame with five year averages by week, country and groupings
#' @export
#'
average_5y <- function(df, groupings = c()) {

  dth_counts_year_week <- dplyr::bind_rows(
    group_count(df, c("year", "week", groupings)),
    group_count(df, c("year", "week", "country", groupings))
  )

  dplyr::summarise(
    dth_counts_year_week,
    avg_5y = sum(deaths) / 5,
    .by = dplyr::any_of(c("week", "area", groupings))
  )
}

#' Calculate percentage change for a value
#'
#' Rounds to dp decimal points
#'
#' @param current Current value
#' @param reference Refence Value
#' @param dp rounds to dp decimal points, default 1 decimal point
#'
#' @return Percentage change rounded to dp decimal points
#' @export
#'
calc_p_change <- function(current, reference, dp = 1) {
  round((current - reference) / reference * 100, dp)
}

#' Format five year average table
#'
#' @param df output of excess_deaths_5ya function
#'
#' @return Formatted data frame.
#' @export
#'
format_excess_deaths_5ya <- function(df) {

  lvls <- create_factor_levels()

  df %>%
    dplyr::mutate(
      area = factor(area, levels = lvls$country),
      place_of_death_other = factor(
        place_of_death_other,
        levels = lvls$place_of_death_other)) %>%
    dplyr::arrange(
      desc(week), place_of_death_other, area) %>%
    dplyr::select(
      `Week number` = week,
      `Area of usual residence` = area,
      `Place of occurrence` = place_of_death_other,
      `Number of deaths` = deaths,
      `Five year average` = avg_5y,
      `Change from five year average [%]` = p_change
    )
}
