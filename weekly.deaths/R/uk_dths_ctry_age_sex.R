#' Combines England and Wales, Scotland and Northern Ireland and calculate UK
#' counts of deaths by country, age groups and sex
#'
#' @description Filters out any data outside of the analysis time period and
#' combines datasets for England and Wales, Scotland and Northern Ireland. Then
#' counts deaths by week, country, age and sex. If no deaths occur in a group,
#' counts of 0 are added. If the scope is EW then there are no rows for UK, NI,
#' Scot for the latest week.
#'
#' @details Adds a column of 1 for `deaths` for England and Wales as the data is
#' record-level. Drops Scotland counts for deaths involving covid to leave "all
#' deaths" as covid are duplicated counts.
#'
#' @param ew_df Processed England and Wales dataframe.
#' @param scot_df Processed Scotland dataframe.
#' @param ni_df Processed Northern Ireland dataframe.
#' @param current_year Analysis year as numeric in format YYYY.
#' @param current_week Week for analysis as numeric or string e.g. 1
#' @param scope string of "UK" or "EW",
#' @param age_group string of age group, default is "agegrp_wide"
#' @return Dataframe of UK counts of deaths by country, age groups and sex.
#' @export
create_uk_dths_ctry_age_sex <- function(ew_df,
                                 scot_df,
                                 ni_df,
                                 current_year,
                                 current_week,
                                 scope,
                                 age_group = "agegrp_wide") {

  uk_deaths <- create_uk_df(ew_df, scot_df, ni_df, current_year)

  uk_deaths <- uk_deaths %>%
    dplyr::select("year", "week", "dor", "sex", {{age_group}},
                  "country", "place_of_death", "deaths") %>%
    count_uk_dths_ctry_age_sex(age_group) %>%
    add_zero_counts_uk(age_group)

  if (scope == "EW") {
    uk_deaths <- dplyr::filter(
      uk_deaths,
      !(country %in% c("Scotland", "Northern Ireland", "UK")
        & week == as.numeric(current_week)
        & year == max(as.numeric(current_year), as.numeric(uk_deaths$year)))
    )
  }

  uk_deaths

}


#' Process UK country ages and filter to period of interest
#'
#' @description Processes the England/Wales (EW), Scotland and Northern Ireland
#' dataframes to be in publishing ready age groups (e.g. "15 to 19").
#' Adds a column of 1 for `deaths` for England and Wales as the data is
#' record-level. Drops Scotland counts for deaths involving covid to leave "all
#' deaths" as covid are duplicated counts. Uses Scotland week definition if
#' necessary.
#' Binds all deaths records together.
#'
#' @details The import and selection of single year of England + Wales data
#' already should already be filtered to the single year during the processing.
#'
#' @param ew_df Processed England and Wales dataframe.
#' @param scot_df Processed Scotland dataframe.
#' @param ni_df Processed Northern Ireland dataframe.
#' @param current_year Analysis year as numeric in format YYYY.
#'
#' @return UK-level dataframe or error if there is more than one year of data.
#' @export
#'
create_uk_df <- function(ew_df, scot_df, ni_df, current_year) {

  ew_df <- reverse_max_agegrp(ew_df)
  scot_df <- process_agegrp(scot_df, agegroup_2)
  ni_df <- process_agegrp(ni_df, agegroup_2)

  dplyr::bind_rows(
    dplyr::mutate(ew_df, deaths = 1),
    filter_before_year_start(
      dplyr::filter(scot_df, covid == 0), current_year, is_scot = TRUE),
    filter_before_year_start(ni_df, current_year))

}


#' Calculate UK counts of deaths by country, age groups and sex
#'
#' @description Takes the processed dataframe containing all deaths in the UK to
#' counts deaths by year, week, country (including UK and England/Wales/
#' Nonresidents), age group and sex. Totals are calculated for each variable
#' (except year and week).
#'
#' @param data UK processed deaths dataframe.
#' @param age_group string of age group, default is "agegrp_wide"
#' @return Dataframe with counts of deaths by week, country, agegroup and sex.
#' @export
count_uk_dths_ctry_age_sex <- function(data, age_group = "agegrp_wide") {

  data <- data %>%
    dplyr::group_by(dplyr::across(
      dplyr::all_of(c("year", "week", "country", age_group, "sex")))) %>%
    dplyr::summarise(deaths = sum(deaths))

  ew_nonres <- data %>%
    # Drop NI and Scot to calculate Eng + Wales + Nonres totals
    dplyr::filter(country %in% c("England", "Wales", "Nonresident")) %>%
    total_by_group(c("sex", age_group), "country",
                   "England, Wales and non-residents")

  uk <- total_by_group(data, c("sex", age_group), "country", "UK")

  # All country totals added to unique breakdown
  ctry_total_added <- dplyr::bind_rows(data, ew_nonres, uk) %>%
    dplyr::filter(country != "Nonresident")

  age_total <- ctry_total_added %>%
    total_by_group(c("sex", "country"), age_group, "All ages")

  sex_total <- ctry_total_added %>%
    total_by_group(c("country", age_group), "sex", "All people")

  sex_age_total <- ctry_total_added %>%
    total_by_group(c("country", age_group), "sex", "All people") %>%
    total_by_group(c("sex", "country"), age_group, "All ages")

  dplyr::bind_rows(
    ctry_total_added, age_total, sex_total, sex_age_total) %>%
    dplyr::ungroup()
}


#' Calculates totals based on given groups (inc. year, week)
#'
#' @description Takes an input of chosen grouping variables (plus `year` and
#' `week`) and then creates rows for the totals of each group combination.
#' Defines the column to total by and what should the total rows be "called".
#' Optional argument can be given to define the numeric column column name to
#' sum by, default is `deaths`.
#'
#' @details Column order of returned dataframe is dependent on grouping_cols and
#' total_col arguments.
#'
#' @param data Long format dataframe with `year` and `week`, plus any
#' columns to group by and total to create for.
#' @param grouping_cols Vector of strings for column names to group by.
#' Note: `year` and `week` are always grouping variables so do
#' not need to be included in this vector.
#' @param total_col String of column name to total by.
#' @param total_name String of what to populate the total column with.
#' @param sum_colname String of the column to sum, default is "deaths", other
#' option could be "population".
#'
#' @return Dataframe of the total counts of deaths.
#' @export
#'
total_by_group <- function(data, grouping_cols, total_col, total_name,
                           sum_colname = "deaths") {

  grouping_cols <- c("year", "week", grouping_cols)

  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) %>%
    dplyr::summarise("{total_col}" := total_name,
                     "{sum_colname}" := sum(.data[[sum_colname]], na.rm = TRUE))
}


#' Format UK counts of deaths by country, agegroups and sex
#'
#' @param data Output dataframe of `create_uk_dths_ctry_age_sex`.
#'
#' @return Dataframe of UK counts of deaths by country, agegroups and sex,
#' formatted for publishing.
#' @export
#'
format_uk_dths_ctry_age_sex <- function(data) {
  lvls <- create_factor_levels()

  data %>%
    dplyr::mutate(
      Country = factor(country, levels = lvls$country),
      `Age group (years)` = factor(agegrp_wide, levels = lvls$agegrp_wide),
      Sex = factor(sex, levels = lvls$sex)) %>%
    dplyr::select(`Week number` = week,
                  Country,
                  `Age group (years)`,
                  Sex,
                  `Number of deaths` = deaths) %>%
    dplyr::arrange(-`Week number`, `Age group (years)`, Sex, Country)
}


#' Add zero counts of death to UK country, age, sex breakdown
#'
#' @description Adds missing combinations for function creating death counts
#' with year, week, country, age and sex.
#'
#' @details This will also use the week number to complete any missing week
#' ending dates.
#'
#' @param data Data frame with following columns:
#' `year`, `week`, `country`, `sex` and `deaths`.
#' @param age_group string of age group, default is "agegrp_wide"
#' @return Data frame with zero deaths for missing combinations.
#' @export
#'
add_zero_counts_uk <- function(data, age_group = "agegrp_wide") {
  data %>%
    tidyr::complete(
      year, week, country, sex, !!!rlang::syms(age_group),
      fill = list(deaths = 0),
      explicit = FALSE)
}
