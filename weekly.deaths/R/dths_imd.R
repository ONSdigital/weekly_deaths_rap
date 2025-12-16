#' Count deaths by IMD quantile
#'
#' @param data Deaths data frame.
#' @param current_week Week for analysis as numeric or string e.g. 1.
#' @param current_year Year for analysis as numeric e.g. 2024.
#' @param imd_lookup IMD lookup data frame.
#' @param quantile String of name of quantile for IMD, either "quintile" or
#' "decile".
#' @param country String of name of country, either "England" or "Wales".
#'
#' @return Formatted data frame.
#' @export
#'
count_imd <- function(data,
                      current_week,
                      current_year,
                      imd_lookup,
                      quantile,
                      country_var) {

  if (!(quantile %in% c("decile", "quintile"))) {
    stop('quantile argument must be "decile" or "quintile"')
  }

  if (!(country_var %in% c("England", "Wales"))) {
    stop('country argument must be "England" or "Wales"')
  }

  imd_col <- paste0("imd_", quantile)

  if (quantile == "decile") {
    imd_vals <- paste0("Decile ", 1:10)
  } else {
    imd_vals <- paste0("Quintile ", 1:5)
  }

  data <- data %>%
    dplyr::filter(country == country_var,
                  week <= as.numeric(current_week),
                  year == current_year) %>%
    dplyr::left_join(imd_lookup, by = dplyr::join_by(lsoa11 == lsoa11cd)) %>%
    dplyr::mutate(
      {{ imd_col }} := paste0(
        stringr::str_to_title(quantile), " ", !!rlang::sym(imd_col)),
      {{ imd_col }} := factor(!!rlang::sym(imd_col), imd_vals),
      agegrp_5yr = factor(agegrp_5yr, create_factor_levels()$agegrp_5yr[-1]))

  totals <- data %>%
    group_by_summarise(c("week", "week_ending", imd_col)) %>%
    dplyr::mutate(agegrp_5yr = "All ages")

  data %>%
    group_by_summarise(c("week", "week_ending", "agegrp_5yr", imd_col)) %>%
    dplyr::bind_rows(totals) %>%
    dplyr::rename(imd = {{ imd_col }}) %>%
    dplyr::mutate(area = country_var)

}

#' Bind IMD values onto table
#'
#' @param table_data Data frame of deaths by area, sex, age group and place of
#' death.
#' @param deaths_data Deaths data frame.
#' @param current_week Week for analysis as numeric or string e.g. 1.
#' @param current_year Year for analysis as numeric e.g. 2024.
#' @param imd_list list containing items england and wales, the imd lookups
#'
#' @return Data frame.
#' @export
#'
bind_imd_values <- function(table_data,
                            deaths_data,
                            current_week,
                            current_year,
                            imd_list) {

  imd <- deaths_data %>%
    count_imd(current_week,
              current_year,
              imd_list$england,
              "decile",
              "England") %>%
    dplyr::bind_rows(count_imd(deaths_data, current_week, current_year,
                               imd_list$wales, "quintile", "Wales")) %>%
    dplyr::mutate(sex = "All people", place_of_death = "All places") %>%
    dplyr::rename(deaths = count)

  table_data %>%
    dplyr::mutate(imd = "All groups") %>%
    dplyr::bind_rows(imd)
}

#' Add most or least deprived label onto IMD factors
#'
#' @param data Formatted data frame, with `IMD quantile group` column.
#'
#' @return Data frame with `IMD quantile group` edited to include most/least
#' deprived tags where applicable.
#' @export
#'
add_most_least_deprived_label <- function(data) {

  data %>%
    dplyr::mutate(`IMD quantile group` = dplyr::case_match(
      `IMD quantile group`,
      "Decile 1" ~ "Decile 1 - most deprived",
      "Decile 10" ~ "Decile 10 - least deprived",
      "Quintile 1" ~ "Quintile 1 - most deprived",
      "Quintile 5" ~ "Quintile 5 - least deprived",
      .default = `IMD quantile group`))
}
