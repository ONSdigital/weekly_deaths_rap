#' Process excess deaths
#' @details Selects the columns required, converts area to full name, processes
#' the age group names, creates the new age bands, sums across the new age
#' groups and filters for the year and weeks required for the publication.
#' @param data dataframe of excess death data
#' @param year numeric in format YYYY
#' @param week numeric of current week number
#'
#' @return dataframe of excess data formatted and prepared
#' @export
#'
process_excess_deaths <- function(data, year, week) {
  data %>%
    dplyr::mutate(area = replace(area, area == "United Kingdom", "UK")) %>%
    dplyr::filter(dor_year == year,
                  dor_week <= week,
                  area %in% create_factor_levels()$country) %>%
    dplyr::select(dor_week, expected_deaths, area, sex, esp_age_group) %>%
    process_agegrp(esp_age_group) %>%
    create_excess_age_groups(esp_age_group) %>%
    dplyr::group_by(dor_week, area, sex, agegrp_excess) %>%
    dplyr::summarise(
      expected_deaths = sum(expected_deaths)) %>%
    dplyr::ungroup()
}

#' Creates excess death age groups
#' @details Converts esp/5 year bands to excess death bands, these are 0 to 29,
#' 30 to 44, 45-49, 50-59 etc.
#' @param data dataframe containing data to be reorganised
#' @param age_column string of the age column's name
#'
#' @return dataframe with new column agegrp_excess
#' @export
#'
create_excess_age_groups <- function(data, age_column) {
  dplyr::mutate(
    data,
    agegrp_excess = dplyr::case_match(
      {{ age_column }},
      "Under 1" ~ "0 to 29",
      "1 to 4" ~ "0 to 29",
      "5 to 9" ~ "0 to 29",
      "10 to 14" ~ "0 to 29",
      "15 to 19" ~ "0 to 29",
      "20 to 24" ~ "0 to 29",
      "25 to 29" ~ "0 to 29",
      "30 to 34" ~ "30 to 44",
      "35 to 39" ~ "30 to 44",
      "40 to 44" ~ "30 to 44",
      .default = {{ age_column }}))
}


#' Creates table of excess deaths
#' @description Creates excess deaths table by creating counts, joining with
#' expected deaths data and adding percentage change
#'
#' @param data Data frame with columns week, area, sex, agegrp_excess, deaths,
#' week_ending
#' @param excess_death_data dataframe with columns dor_year, dor_week, area,
#' sex, deaths, expected_deaths
#' @param year numeric year for analysis (YYYY)
#' @param week numeric week for analysis
#'
#' @return Data frame with columns as data dataframe and expected_deaths
#'
#' @export
#'
calc_excess_deaths <- function(data, excess_death_data, year, week) {
  data %>%
    count_dths_geo_age_sex(year, week) %>%
    dplyr::left_join(
      excess_death_data,
      by = dplyr::join_by(week == dor_week, area, sex, agegrp_excess)) %>%
    dplyr::mutate(
      p_change = calc_p_change(deaths, expected_deaths),
      # percentage change needs to be computed before rounding
      expected_deaths = round(expected_deaths, 0),
      excess_deaths = deaths - expected_deaths)
}

#' Creates table of excess deaths for all countries of the UK
#' @description Creates excess deaths table by creating counts, joining with
#' expected deaths data and adding percentage change
#'
#' @param data Data frame with columns week, area, sex, agegrp_excess, deaths,
#' week_ending
#' @param excess_death_data frame with columns dor_year, dor_week, area, sex,
#' deaths, expected_deaths
#' @param config list containing week (numeric week for analysis) and scope
#' (string of "UK" or "EW")
#'
#' @return Data frame with columns as data dataframe and expected_deaths
#'
#' @export
#'
calc_excess_deaths_uk <- function(data,
                                  excess_death_data,
                                  config) {
  data <- data %>%
    create_excess_age_groups(agegroup_2) %>%
    dplyr::group_by(week, country, sex, agegrp_excess) %>%
    dplyr::summarise(
      deaths = sum(deaths)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      excess_death_data,
      by = dplyr::join_by(week == dor_week,
                          country == area,
                          sex,
                          agegrp_excess)) %>%
    dplyr::mutate(
      p_change = calc_p_change(deaths, expected_deaths),
      # percentage change needs to be computed before rounding
      expected_deaths = round(expected_deaths, 0),
      excess_deaths = deaths - expected_deaths)

  if (config$scope == "EW") {
    data <- dplyr::filter(data,
      !(country %in% c("Scotland", "Northern Ireland", "UK")
      & week == as.numeric(config$week))
    )
  }
  data
}

#' Count weekly deaths by area, age and sex
#'
#' @description Counts deaths by week, country, age group, sex
#'
#' @details
#' Filtering occurs after this, to only include the publication year and weeks
#' required up to and including the publication week. Filtering for dor took
#' place in the processing but an additional filtering needed to remove cases
#' where dod was in the previous year, which can only take place after year,
#' week and week_ending calculated based on dod.
#' 0 counts are added for any missing combinations of factors. However, when 0
#' counts are added, week ending date is not completed so must be completed by
#' referring to the week number.
#'
#' @param df Data frame of death records with following columns:
#' dor, dod, country, region, agegrp_excess, sex and
#' @param current_year Year for analysis as numeric e.g. 2024
#' @param current_week Week for analysis as numeric or string e.g. 1
#'
#' @return A dataframe containing the summarised death counts.
#'
#' @export
#'
count_dths_geo_age_sex <- function(df, current_year, current_week) {

  df <- df %>%
    flag_agegrp_excess(ageinyrs) %>%
    dplyr::filter(year == current_year, week <= as.numeric(current_week)) %>%
    check_columns_present(c("week", "week_ending", "year", "country", "region",
                            "agegrp_excess", "sex"))

  deaths <- dplyr::bind_rows(
    group_count_geo_country(df),
    group_count_geo_country(df, "sex"),
    group_count_geo_country(df, "agegrp_excess"),
    group_count_geo_country(df, c("sex", "agegrp_excess"))
  )

  deaths$sex[is.na(deaths$sex)] <- "All people"
  deaths$agegrp_excess[is.na(deaths$agegrp_excess)] <- "All ages"

  deaths %>%
    add_zero_counts_no_pod() %>%
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
group_count_geo_country <- function(df, groupings = c()) {
  dplyr::bind_rows(
    group_count(df, groupings),
    group_count(df, c("country", groupings))
  )
}


#' Add zero counts to age, sex, area, place breakdowns
#'
#' @description Adds missing combinations for function creating death counts
#' with week, area, sex, age and place of death breakdowns. Note that not all
#' combinations are included, so this completes the function for the following:
#' week x area x age x sex AND
#' week x area
#'
#' @param data Data frame with following columns:
#' week, area, sex, agegrp_excess
#'
#' @return Data frame with zero deaths for missing combinations.
#' @export
#'
add_zero_counts_no_pod <- function(data) {

  data %>%
    tidyr::complete(
      week,
      area,
      fill = list(deaths = 0, sex = "All people", agegrp_excess = "All ages"),
      explicit = FALSE) %>%
    tidyr::complete(
      week,
      area,
      sex,
      agegrp_excess,
      fill = list(deaths = 0),
      explicit = FALSE)
}


#' Format weekly deaths by country, age, sex, and place of death.
#'
#' @param data Data frame with week, country_region, agegrp_excess, sex,
#' place_of_death, and count columns.
#'
#' @return Formatted data frame.
#' @export
#'
format_excess_deaths_model <- function(data) {

  lvl <- create_factor_levels()

  data %>%
    dplyr::mutate(
      area = factor(country, levels = lvl$country),
      sex = factor(sex, levels = lvl$sex),
      agegrp_excess = factor(agegrp_excess, levels = lvl$agegrp_excess)) %>%
    dplyr::arrange(desc(week), agegrp_excess, sex, area) %>%
    dplyr::filter(!(area %in%
                      c("Wales", "Scotland", "England", "Northern Ireland") &
                    agegrp_excess != "All ages")) %>%
    dplyr::select(
      `Week number` = week,
      `Country` = area,
      `Sex` = sex,
      `Age group (years)` = agegrp_excess,
      `Number of deaths` = deaths,
      `Expected deaths` = expected_deaths,
      `Excess deaths` = excess_deaths,
      `Change from expected deaths [%]` = p_change)
}
