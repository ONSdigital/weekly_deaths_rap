#' Count weekly deaths by area, age, sex, and place of death
#'
#' @description Counts deaths by week, country, region, age group, sex, and
#' place of death.
#'
#' @details Filtering occurs to only include the publication year and weeks
#' required up to and including the publication week. Filtering for dor took
#' place in the processing but an additional filtering needed to remove cases
#' where dod was in the previous year, which can only take place after year,
#' week and week_ending calculated based on dod.
#' 0 counts are added for any missing combinations of factors. However, when 0
#' counts are added, week ending date is not completed so must be completed by
#' referring to the week number.
#'
#' @param df Data frame of death records with following columns:
#' dor, dod, country, region, agegroup_5yr, sex and place_of_death.
#' @param current_year Year for analysis as numeric e.g. 2024.
#' @param current_week Week for analysis as numeric or string e.g. 1.
#'
#' @return A data frame containing the summarised death counts.
#'
#' @export
#'
count_dths_geo_age_sex_pod <- function(df,
                                       current_year,
                                       current_week) {

  lvls <- create_factor_levels()

  df <- df %>%
    dplyr::filter(year == current_year, week <= as.numeric(current_week)) %>%
    check_columns_present(c("week", "week_ending", "year", "country", "region",
                            "agegrp_5yr", "sex", "place_of_death")) %>%
    dplyr::mutate(
      agegrp_5yr = factor(agegrp_5yr, lvls$agegrp_5yr[-1]),
      sex = factor(sex, lvls$sex[-1]),
      place_of_death = factor(place_of_death, lvls$place_of_death[-1]))

  deaths <- dplyr::bind_rows(
    group_count_all_geo(df),
    group_count_all_geo(df, "sex"),
    group_count_all_geo(df, "agegrp_5yr"),
    group_count_all_geo(df, c("sex", "agegrp_5yr")),
    group_count_all_geo(df, "place_of_death"))

  deaths %>%
    dplyr::mutate(
      agegrp_5yr = factor(dplyr::if_else(
        is.na(agegrp_5yr), "All ages", agegrp_5yr), lvls$agegrp_5yr),
      sex = factor(dplyr::if_else(is.na(sex), "All people", sex), lvls$sex),
      place_of_death = factor(dplyr::if_else(
        is.na(place_of_death), "All places", place_of_death),
        lvls$place_of_death)) %>%
    add_zero_counts() %>%
    week_ending_from_week()
}

#' Count death records by group and area in England and Wales
#'
#' Counts deaths records by specified groupings for following geographies:
#' England, Wales and non-residents, England, Wales, English regions.
#'
#' @param df Data frame with 1 row equivalent to 1 death. Has to include region
#' and country columns and the desired groupings, specified in groupings
#' argument.
#' @param groupings Vector of column names to group by as strings.
#'
#' @return Data frame with number of deaths by area and specified groupings.
#' @export
#'
group_count_all_geo <- function(df, groupings = c()) {
  dplyr::bind_rows(
    group_count(df, groupings),
    group_count(df, c("country", groupings)),
    group_count(df, c("region", groupings))
  )
}

#' Count deaths records by group
#'
#' Counts deaths records by group, excludes non-residents if groupings include
#' country, excludes non-residents and Wales if groupings include regions. If
#' neither country or region groupings are specified, area is assumed to  be
#' England, Wales and non-residents.
#'
#' @param df Data frame with 1 row equivalent to 1 death. Has to include the
#' desired groupings, specified in groupings argument.
#' @param groupings Vector of column names to group by as strings.
#'
#' @return Data frame with number of deaths by desired groupings.
#' @export
#'
group_count <- function(df, groupings = c()) {

  counts <- df %>%
    dplyr::group_by(
      dplyr::across(dplyr::any_of(c("week", "week_ending", groupings))),
      .drop = FALSE) %>%
    dplyr::summarise(
      deaths = dplyr::n()) %>%
    dplyr::ungroup() %>%
    data.frame()

  if ("region" %in% groupings) {
    counts <- counts %>%
      dplyr::filter(region != "Wales", region != "Nonresident") %>%
      dplyr::rename(area = region)
  } else if ("country" %in% groupings) {
    counts <- counts %>%
      dplyr::filter(country != "Nonresident") %>%
      dplyr::rename(area = country)
  } else {
    counts$area <- "England, Wales and non-residents"
  }
  counts
}

#' Add zero counts to age, sex, area, place breakdowns
#'
#' @description Adds missing combinations for function creating death counts
#' with week, area, sex, age and place of death breakdowns. Note that not all
#' combinations are included, so this completes the function for the following:
#' week x area x age x sex AND
#' week x area x place of death
#'
#' @details NOTE: This does *NOT* work for combinations of variables that do not
#' exist at all (e.g. there are 0 deaths in age group 25 to 29). It only works
#' if there is at least one of the combinations. It must be used with care and
#' preferably following factors.
#'
#' @param data Data frame with following columns:
#' `week`, `area`, `sex`, `agegrp_5yr`, `place_of_death`
#'
#' @return Data frame with zero deaths for missing combinations.
#' @export
#'
add_zero_counts <- function(data) {

  data %>%
    tidyr::complete(
      week, area, place_of_death,
      fill = list(deaths = 0, sex = "All people", agegrp_5yr = "All ages"),
      explicit = FALSE) %>%
    tidyr::complete(
      week, area, sex, agegrp_5yr,
      fill = list(deaths = 0, place_of_death = "All places"),
      explicit = FALSE)
}


#' Uses existing combinations of week and week ending to calculate missing week
#' ending dates
#'
#' @description Due to adding counts of 0, some week_ending are NA and must be
#' re-populated with week_ending derived from the week number.
#'
#' @param data Processed deaths counts dataframe with week and week_ending
#' columns.
#'
#' @return Original dataframe with week_ending column recreated and added to
#' end.
#' @export
#'
week_ending_from_week <- function(data) {
  data %>%
    dplyr::select(-week_ending) %>%
    dplyr::left_join(unique(na.omit(data[, c("week", "week_ending")])),
                     by = "week",
                     relationship = "many-to-many")
}

#' Format weekly deaths by country/region, age, sex, and place of death.
#'
#' @param data Data frame with week, country_region, agegrp_5yr, sex, imd,
#' place_of_death, and count columns.
#'
#' @return Formatted data frame.
#' @export
#'
format_dths_geog_age_sex_pod_imd <- function(data) {

  lvl <- create_factor_levels()

  data %>%
    dplyr::mutate(
      area = factor(area, levels = lvl$country_region),
      sex = factor(sex, levels = lvl$sex),
      agegrp_5yr = factor(agegrp_5yr, levels = lvl$agegrp_5yr),
      place_of_death = factor(place_of_death, levels = lvl$place_of_death),
      imd = factor(imd, levels = lvl$imd)) %>%
    dplyr::arrange(
      desc(week), place_of_death, imd, agegrp_5yr, sex, area) %>%
    dplyr::select(
      `Week number` = week,
      `Week ending` = week_ending,
      `Area of usual residence` = area,
      `Sex` = sex,
      `Age group (years)` = agegrp_5yr,
      `IMD quantile group` = imd,
      `Place of occurrence` = place_of_death,
      `Number of deaths` = deaths) %>%
    add_most_least_deprived_label()
}
