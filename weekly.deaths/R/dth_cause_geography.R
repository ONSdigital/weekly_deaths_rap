#' Create table containing death totals for each cause, per region, per week
#'
#' @details Developed using just England, Wales and Non-resident data, so
#' untested with Scotland and NI in mind, should this become relevant. It will
#' complete any missing cases with 0 values.
#'
#' @description From the weekly deaths dataset, derives the number of deaths per
#' cause, on a per region level and all of this for each week in the year.
#'
#' @param data Dataframe of the deaths dataset
#'
#' @return Dataframe containing death nums for causes, by region, per week
#' @export
#'
count_dths_cause_geo <- function(data) {

  data <- data %>%
    flag_weekly_causes() %>%
    dplyr::select(week, week_ending, country, region, dplyr::contains("inv"),
                  dplyr::contains("due")) %>%
    prepare_regions_for_analysis()

  deaths_all_causes <- data %>%
    dplyr::mutate(cause = "All causes") %>%
    dplyr::count(week, week_ending, region, cause)

   rbind(
     deaths_all_causes,
     count_dths_cause_region(data)) %>%
     registration_cause_zero_values()
}


#' Derive the different regional breakdowns
#'
#' @description From the regional and country columns, create the various levels
#' of granularity by combining country and regional into a singular region
#' column. For instance, merging the regions of England, into a single England
#' region and merging of the England, Wales, and Non-residents, into a region
#' of the same name.
#' Nonresidents are only included in the overall region of 'England, Wales and
#' Non-residents', but not England, Wales or individual English regions, and
#' don't have  their own separate regional breakdown.
#'
#' @param data Dataframe being used for analysis
#'
#' @return Dataframe containing additional regional breakdowns
#' @export
#'
prepare_regions_for_analysis <- function(data) {

  data_eng_wal_non <- dplyr::mutate(
    data,
    country = "England, Wales and non-residents",
    region = "England, Wales and non-residents")

  # Only want non-residents in overall category, rather than on their own
  data <- dplyr::filter(data, country != "Nonresident")

  data_eng <- data %>%
    dplyr::filter(country == "England") %>%
    dplyr::mutate(region = country)

  rbind(
    data_eng_wal_non,
    data_eng,
    data)
}

#' Count deaths for each cause flag
#'
#' @description For each cause flag, go through the process of grouping them
#' together by week and region so that we have a total number of deaths.
#' Then, join them all together and structure them so that we can have this
#' stored in a common cause and number column, rather than the original
#' structure of a column per flag.
#'
#' @param data Dataframe containing the death data
#'
#' @return Dataframe containing all causes and their attributed number of deaths
#' per week, by region
#' @export
#'
count_dths_cause_region <- function(data) {

  cause_dict <- list(
    resp_inv =
      "Deaths involving diseases of the respiratory system (J00 to J99)",
    resp_due = "Deaths due to diseases of the respiratory system (J00 to J99)",
    infl_inv = "Deaths involving influenza or pneumonia (J09 to J18)",
    infl_due = "Deaths due to influenza or pneumonia (J09 to J18)",
    covid_inv = "Deaths involving COVID-19 (U07.1, U07.2, U09.9, U10.9)",
    covid_due = "Deaths due to COVID-19 (U07.1, U07.2, U10.9)"
  )

  missing_cols <- names(cause_dict)[!(names(cause_dict) %in% colnames(data))]

  if (length(missing_cols) > 0) {
    error_output <- "Missing column(s) given to count_dths_cause_region,"
    missing_cols <- paste(missing_cols, collapse = ", ")
    error_output <- paste0(error_output, " these are: ", missing_cols)
    stop(error_output)
  }

  dth_cause <- NULL

  for (curr_dth in names(cause_dict)) {

    curr_table_cause <- data %>%
      dplyr::rename(cause = {{curr_dth}}) %>%
      dplyr::count(week, week_ending, region, cause) %>%
      dplyr::filter(cause == TRUE) %>%
      dplyr::mutate(cause = cause_dict[[curr_dth]])

    dth_cause <- rbind(dth_cause, curr_table_cause)
  }

  dth_cause
}


#' Handle final formating for deaths per cause, per region, by week table
#'
#' @description Perform final formatting on the table to get the data into a
#' more publishable state.
#' Such as:
#' • Factorising relevant columns
#' • Ordering of data
#' • Ordering of columns
#' • Renaming column to something more human-readable
#'
#' @param data Dataset containing the output from the
#' create_tbl_dths_per_cause_and_regn_by_wk() function
#'
#' @return Dataset containing the formatted output from the
#' create_tbl_dths_per_cause_and_regn_by_wk()
#' @export
#'
format_dths_cause_geo <- function(data) {

  lvls <- create_factor_levels()

  data <- data %>%
    dplyr::mutate(
      region = factor(region, levels = lvls$country_region),
      cause = factor(cause, levels = lvls$causes)) %>%
    dplyr::arrange(dplyr::desc(week), week_ending, region, cause, n) %>%
    dplyr::select(
      `Week number` = week,
      `Week ending` = week_ending,
      `Area of usual residence` = region,
      `Cause of death` = cause,
      `Number of deaths` = n)
}

#' Identifies missing values in by cause of death columns and replaces with
#' 0 counts
#'
#' @description Assesses the England, Wales, and non-residents cause of death
#' data frame to identify and replace date columns where there are no deaths
#' recorded for that week by certain causes
#'
#' @param data Data frame with week, week ending, region, cause of death,
#' counts
#'
#' @return Dataframe containing death nums for causes, by region, per week,
#' including 0 counts
#' @export
#'
registration_cause_zero_values <- function(data) {

  data %>%
    tidyr::complete(
      tidyr::nesting(week, week_ending),
      region,
      cause,
      fill = list(n = 0))
}
