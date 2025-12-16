#' Create df with info about whether table lengths are as expected
#'
#' @param reg_table data frame containing death registration data
#' @param occ_table data frame containing death occurrences data
#' @param cause_table data frame containing cause of death data
#' @param delay_table data frame containing death registration delays data
#' @param excess_table data frame containing excess death data
#' @param uk_table data frame containing UK death registration data
#' @param asmr_table data frame containing UK ASMR data
#' @param weeks week for analysis as numeric or string e.g. 1
#'
#' @return A data frame containing the table number, the table's data type,
#' the number of rows in table, the number of rows that are expected in the
#' table and a matching column to indicate if the table is as expected.
#' @export
#'
create_lengths_table <- function(reg_table,
                                 occ_table,
                                 cause_table,
                                 delay_table,
                                 excess_table,
                                 uk_table,
                                 asmr_table,
                                 weeks,
                                 scope,
                                 previous_year) {

  if (!exists(deparse(substitute(reg_table)))) {
    reg_table <- data.frame()
  }
  if (!exists(deparse(substitute(occ_table)))) {
    occ_table <- data.frame()
  }
  if (!exists(deparse(substitute(cause_table)))) {
    cause_table <- data.frame()
  }
  if (!exists(deparse(substitute(delay_table)))) {
    delay_table <- data.frame()
  }
  if (!exists(deparse(substitute(excess_table)))) {
    excess_table <- data.frame()
  }
  if (!exists(deparse(substitute(uk_table)))) {
    uk_table <- data.frame()
  }
  if (!exists(deparse(substitute(asmr_table)))) {
    asmr_table <- data.frame()
  }

  weeks <- as.numeric(weeks)
  rows_in_previous_year <- calc_weeks_in_year(as.numeric(previous_year)) * 1233

  actual_lengths <- c(nrow(reg_table), nrow(occ_table), nrow(cause_table),
                      nrow(delay_table), nrow(excess_table), nrow(uk_table),
                      nrow(asmr_table))

  lengths_df <- data.frame(
    table_num = c(
      "Table 1",
      "Table 2",
      "Table 3",
      "Table 4",
      "Table 5",
      "Table 6",
      "Table 7"
      ),
    table_name = c(
      "Registrations",
      "Occurrences",
      "By Cause",
      "Delays",
      "Excess deaths",
      "UK deaths",
      "ASMRs"),
    single_week_length = c(1233, 1233, 84, 504, 90, 144, 18),
    number_rows = actual_lengths
  )

  lengths_df %>% dplyr::mutate(
    expected_length = single_week_length * weeks,
    expected_length = dplyr::case_when(
      scope == "EW" & table_name == "UK deaths" ~ expected_length - 72,
      scope == "EW" & table_name == "Excess deaths" ~ expected_length - 45,
      table_name == "Occurrences" ~ expected_length + rows_in_previous_year,
      TRUE ~ expected_length),
    match = ifelse(number_rows == 0,
                   "Check table exists",
                   expected_length == number_rows),
    match = kableExtra::cell_spec(
      match,
      "html",
      background = ifelse(match == TRUE, "lightgreen",
                          ifelse(match == FALSE, "red", "yellow")))
  ) %>%
    dplyr::select(-single_week_length) %>%
    dplyr::rename(
      `Table number` = table_num,
      `Data` = table_name,
      `Number of rows` = number_rows,
      `Expected rows` = expected_length,
      `Match` = match
    ) %>%
    kableExtra::kbl(format = "html", escape = FALSE) %>%
    kableExtra::kable_paper(c("striped", "hover"),
                            full_width = FALSE,
                            position = "left",
                            html_font = "Arial")
}


#' @title subtotals_check
#'
#' @description
#' Checks whether tables exists, calculates information for doing subgroups
#' checks and checks that the subtotals sum to the correct overall value
#'
#' @param data The R dataframe in the global environment containing the
#' relevant data.
#' @param data_type string identifying the type of data ("registrations",
#' "occurrences")
#' @param topic string of grouping of interest (age, sex, region, place).
#'
#' @returns
#' TRUE or FALSE if table exists and subtotals sum correctly.
#' "Check table exists" if table doesn't exist.
#' Nothing will be saved to the global environment by running the function
#' alone.
#'
#' @examples
#' x <- subtotals_check(ew_registrations, "registrations", "age")
#'
#' @export

subtotals_check <- function(data, data_type, topic) {

  x <- deparse(substitute(data))
  if (!exists(x)) {
    "Check table exists"
  } else {
    subgroup_info <- get_subgroup_info(data, x, topic)
    data <- subgroup_info$data
    compare_totals(
      data, data_type, subgroup_info$var, subgroup_info$groupings,
      subgroup_info$vars, subgroup_info$overall)

  }
}

#' @title compare_totals
#'
#' @description
#' Checks whether subgroups sum up to totals for all relevant tables in ONS
#' weekly deaths release. Intended for use in the production of a quality
#' report.
#'
#' @details
#' If the sum of subgroups on a table do not equal the total then the final
#' variable is set to FALSE. If the sum of subgroups on a table equal the total
#' then the final variable is set to YES.
#'
#' @param data dataframe containing number of deaths
#' @param data_type string identifying the type of data ("registrations",
#' "occurrences")
#' @param var string of the variable of interest
#' @param groupings vector groupings of var, e.g. Female, Male for sex.
#' @param grouping_vars vector of groupings to check sums for (age, sex, region)
#' @param overall string of the overall category e.g. All people for sex
#' @returns TRUE or FALSE if table exists and subtotals sum correctly.
#'
#' @export
compare_totals <- function(data,
                           data_type,
                           var,
                           groupings,
                           grouping_vars,
                           overall) {

  if (data_type == "registrations") {
    df1 <- data %>%
      dplyr::filter(.data[[var]] %in% groupings) %>%
      dplyr::group_by(week_number,
                      dplyr::across(dplyr::all_of(grouping_vars))) %>%
      dplyr::summarise(sum_deaths = sum(number_of_deaths), .groups = "keep")

    df2 <- data %>%
      dplyr::filter(.data[[var]] == overall) %>%
      dplyr::full_join(df1, by = c("week_number", grouping_vars)) %>%
      dplyr::mutate(
        check = dplyr::case_when(is.na(number_of_deaths) |
                                   is.na(sum_deaths) ~ 1,
                                 number_of_deaths != sum_deaths ~ 1,
                                 number_of_deaths == sum_deaths ~ 0)) %>%
      dplyr::summarise(check = sum(check))
  } else if (data_type == "occurrences") {
    df1 <- data %>%
      dplyr::filter(.data[[var]] %in% groupings) %>%
      dplyr::group_by(year, week_number,
                      dplyr::across(dplyr::all_of(grouping_vars))) %>%
      dplyr::summarise(sum_deaths = sum(number_of_deaths), .groups = "keep")

    df2 <- data %>%
      dplyr::filter(.data[[var]] == overall) %>%
      dplyr::full_join(df1, by = c("year", "week_number", grouping_vars)) %>%
      dplyr::mutate(
        check = dplyr::case_when(is.na(number_of_deaths) |
                                   is.na(sum_deaths) ~ 1,
                                 number_of_deaths != sum_deaths ~ 1,
                                 number_of_deaths == sum_deaths ~ 0)) %>%
      dplyr::summarise(check = sum(check))
  }

  if (df2$check > 0) {
    x <- FALSE
  } else if (df2$check == 0) {
    x <- TRUE
  }
}


#' @title get_subgroup_info
#'
#' @description Function for creating var the variable that subtotals will be
#' checked for, groupings that make up the var, vars that will be used for the
#' breakdowns, overall for what the groupings should sum to and data which is
#' data filtered as required.
#'
#' @param data dataframe of death data
#' @param x string of dataframe name
#' @param topic string grouping of interest e.g. region, age, sex, place
#'
#' @returns a list with named elements var, groupings, vars, overall, data
#'
#' @export
#'
get_subgroup_info <- function(data, x, topic) {
  data <- data %>% janitor::clean_names("snake")

  if (x %in% c("ew_registrations", "ew_occurrences")) {
    subgroup_info <- get_reg_occ_subgroup_info(data, x, topic)
  } else if (x == "uk_registrations") {
    subgroup_info <- get_uk_reg_subgroup_info(data, x, topic)
  } else if (x == "ew_registrations_by_cause") {
    subgroup_info <- get_cause_subgroup_info(data, x, topic)
  } else if (x == "uk_excess_deaths") {
    subgroup_info <- get_excess_subgroup_info(data, x, topic)
  } else if (x == "ew_reg_delays_cert_pod_area") {
    subgroup_info <- get_delays_subgroup_info(data, x, topic)
  } else {
    warning("The table may not exist. Please check.")
  }

}

#' @title get_reg_occ_subgroup_info
#'
#' @description Function for registration and occurrence tables for creating var
#' variable that subtotals will be checked for, groupings that make up the var,
#' vars that will be used for the breakdowns, overall for what the groupings
#' should sum to and data which is data filtered as required.
#'
#' @param data dataframe of death data
#' @param x string of dataframe name
#' @param topic string - grouping of interest e.g. region, age, sex, place
#'
#' @returns a list with named elements var, groupings, vars, overall, data
#'
#' @export
get_reg_occ_subgroup_info <- function(data, x, topic) {
  lvls <- create_factor_levels()

  if (topic == "region") {
    var <- "area_of_usual_residence"
    groupings <- lvls$region
    vars <- c("sex", "age_group_years", "place_of_occurrence")
    overall <- "England"

    data <- dplyr::filter(data, imd_quantile_group == "All groups")

  } else if (topic == "sex") {

    var <- "sex"
    groupings <- c("Female", "Male")
    vars <- c("area_of_usual_residence", "age_group_years",
              "place_of_occurrence")
    overall <- "All people"

    data <- dplyr::filter(data,
                          place_of_occurrence == "All places",
                          imd_quantile_group == "All groups")

  } else if (topic == "age") {

    var <- "age_group_years"
    groupings <- lvls$agegrp_5yr[lvls$agegrp_5yr != "All ages"]
    vars <- c("area_of_usual_residence", "sex", "place_of_occurrence")
    overall <- "All ages"

    data <- dplyr::filter(data,
                          place_of_occurrence == "All places",
                          imd_quantile_group == "All groups")

  } else if (topic == "place") {

    var <- "place_of_occurrence"
    groupings <- lvls$place_of_death[lvls$place_of_death != "All places"]
    vars <- c("area_of_usual_residence", "sex", "age_group_years")
    overall <- "All places"

    data <- dplyr::filter(data,
                          sex == "All people",
                          age_group_years == "All ages",
                          imd_quantile_group == "All groups")

  } else if (topic == "imd_eng") {

    var <- "imd_quantile_group"
    groupings <- c("Decile 1 - most deprived", paste("Decile", 2:9),
                   "Decile 10 - least deprived")
    vars <- "age_group_years"
    overall <- "All groups"

    data <- dplyr::filter(data,
                          area_of_usual_residence == "England",
                          sex == "All people",
                          place_of_occurrence == "All places")

  } else if (topic == "imd_wales") {

    var <- "imd_quantile_group"
    groupings <- c("Quintile 1 - most deprived", paste("Quintile", 2:4),
                   "Quintile 5 - least deprived")
    vars <- "age_group_years"
    overall <- "All groups"

    data <- dplyr::filter(data,
                          area_of_usual_residence == "Wales",
                          sex == "All people",
                          place_of_occurrence == "All places")

  } else {
    warning("The grouping may not be valid for the table. Please check.")
  }
  list("var" = var, "groupings" = groupings, "vars" = vars, "overall" = overall,
       "data" = data)

}

#' @title get_uk_reg_subgroup_info
#'
#' @description Function for uk registration table for creating var the
#' variable that subtotals will be checked for, groupings that make up the var,
#' vars that will be used for the breakdowns, overall for what the groupings
#' should sum to and data which is data filtered as required.
#'
#' @param data dataframe of death data
#' @param x string of dataframe name
#' @param topic string grouping of interest e.g. region, age, sex, place
#'
#' @returns a list with named elements var, groupings, vars, overall, data
#'
#' @export
get_uk_reg_subgroup_info <- function(data, x, topic) {
  lvls <- create_factor_levels()
  if (topic == "country") {

    var <- "country"
    groupings <- c("England, Wales and non-residents", "Scotland",
                   "Northern Ireland")
    vars <- c("age_group_years", "sex")
    overall <- "UK"

  } else if (topic == "age") {

    var <- "age_group_years"
    groupings <- lvls$agegrp_wide[lvls$agegrp_wide != "All ages"]
    vars <- c("country", "sex")
    overall <- "All ages"

  } else if (topic == "sex") {

    var <- "sex"
    groupings <- c("Female", "Male")
    vars <- c("country", "age_group_years")
    overall <- "All people"
  } else {
    warning("The grouping may not be valid for the table. Please check.")
  }

  list("var" = var, "groupings" = groupings, "vars" = vars, "overall" = overall,
       "data" = data)

}

#' @title get_cause_subgroup_info
#'
#' @description Function for registrations by cause table for creating var the
#' variable that subtotals will be checked for, groupings that make up the var,
#' vars that will be used for the breakdowns, overall for what the groupings
#' should sum to and data which is data filtered as required.
#'
#' @param data dataframe of death data
#' @param x string of dataframe name
#' @param topic string grouping of interest e.g. region, age, sex, place
#'
#' @returns a list with named elements var, groupings, vars, overall, data
#'
#' @export
get_cause_subgroup_info <- function(data, x, topic) {
  lvls <- create_factor_levels()
  if (topic == "region") {

    var <- "area_of_usual_residence"
    groupings <- lvls$region
    vars <- "cause_of_death"
    overall <- "England"
  } else {
    warning("The grouping may not be valid for the table. Please check.")
  }

  list("var" = var, "groupings" = groupings, "vars" = vars, "overall" = overall,
       "data" = data)

}

#' @title get_excess_subgroup_info
#'
#' @description Function forew registrations by cause table for creating var the
#' variable that subtotals will be checked for, groupings that make up the var,
#' vars that will be used for the breakdowns, overall for what the groupings
#' should sum to and data which is data filtered as required.
#'
#' @param data dataframe of death data
#' @param x string of dataframe name
#' @param topic string grouping of interest e.g. region, age, sex, place
#'
#' @returns a list with named elements var, groupings, vars, overall, data
#'
#' @export
get_excess_subgroup_info <- function(data, x, topic) {
  lvls <- create_factor_levels()
  if (topic == "age") {

    var <- "age_group_years"
    groupings <- lvls$agegrp_excess[lvls$agegrp_excess != "All ages"]
    vars <- c("country", "sex")
    overall <- "All ages"

    data <- dplyr::filter(
      data, country %in% c("England, Wales and non-residents", "UK"))
  } else if (topic == "sex") {

    var <- "sex"
    groupings <- c("Female", "Male")
    vars <- c("country", "age_group_years")
    overall <- "All people"
  } else {
    warning("The grouping may not be valid for the table. Please check.")
  }

  list("var" = var, "groupings" = groupings, "vars" = vars, "overall" = overall,
       "data" = data)

}

#' Get subgroup information for delays
#'
#' @description Uses England and Wales delays table to check subtotals against
#' overall total.
#'
#' @param data Data frame of delays.
#' @param x String of data frame name.
#' @param topic String of grouping of interest, e.g., region, place, cert_type.
#'
#' @return A list with named elements var, groupings, vars, overall, data
#' @export
#'
get_delays_subgroup_info <- function(data, x, topic) {
  lvls <- create_factor_levels()

  if (topic == "region") {
    var <- "area_of_usual_residence"
    groupings <- lvls$region
    vars <- c("certification_type", "place_of_occurrence")
    overall <- "England"
  } else if (topic == "place") {
    var <- "place_of_occurrence"
    groupings <- lvls$place_of_death[lvls$place_of_death != "All places"]
    vars <- c("certification_type", "area_of_usual_residence")
    overall <- "All places"

  } else if (topic == "cert_type") {
    var <- "certification_type"
    groupings <- lvls$cert_type[!(
      lvls$cert_type %in% c("All deaths", "Certified by coroner"))]
    vars <- c("area_of_usual_residence", "place_of_occurrence")
    overall <- "All deaths"
    data <- dplyr::filter(data, certification_type != "Certified by coroner")

  } else {
    warning("The grouping may not be valid for the table. Please check.")
  }

  list("var" = var, "groupings" = groupings, "vars" = vars, "overall" = overall,
       "data" = data)
}


#' @title create_subtotals_table
#'
#' @description Function for creating table checking subtotals sum to relevant
#' total for the relevant tables in weekly deaths
#'
#' @param ew_registrations dataframe of death registration data
#' @param ew_occurrences dataframe of death occurrence data
#' @param ew_registrations_by_cause dataframe of death registration data by
#' cause of death
#' @param uk_excess_deaths dataframe of death registration data by excess deaths
#' @param uk_registrations dataframe containing death registration data for
#' England, Wales, Scotland, Northern Ireland and the uK.
#'
#' @returns A knitr::kable for a rmd file
#'
#' @export
#'
create_subtotals_table <- function(ew_registrations,
                                   ew_occurrences,
                                   ew_registrations_by_cause,
                                   ew_reg_delays_cert_pod_area,
                                   uk_excess_deaths,
                                   uk_registrations,
                                   week,
                                   scope) {
  if (scope == "EW") {
    uk_registrations <- dplyr::filter(uk_registrations, `Week number` != week)
  }

  ew_reg_delays_cert_pod_area <- dplyr::rename(ew_reg_delays_cert_pod_area,
                                               "Number of deaths" =
                                                 "Deaths registered in week")

  sub_totals_df <- data.frame(
    Table = c("1 - EW Registrations",
              "1 - EW Registrations",
              "1 - EW Registrations",
              "1 - EW Registrations",
              "1 - EW Registrations",
              "1 - EW Registrations",
              "2 - EW Occurrences",
              "2 - EW Occurrences",
              "2 - EW Occurrences",
              "2 - EW Occurrences",
              "2 - EW Occurrences",
              "2 - EW Occurrences",
              "3 - EW Registrations by cause",
              "4 - EW Registrations by delays",
              "4 - EW Registrations by delays",
              "4 - EW Registrations by delays",
              "5 - Excess deaths",
              "5 - Excess deaths",
              "6 - UK registrations",
              "6 - UK registrations",
              "6 - UK registrations"),
    Check = c("English regions sum to England total",
              "Age groups sum to all ages total",
              "Female and male counts sum to all people total",
              "Place of death groups sum to all people total",
              "English IMD deciles sum to England total",
              "Welsh IMD quintiles sum to Wales total",
              "English regions sum to England total",
              "Age groups sum to all ages total",
              "Female and male counts sum to all people total",
              "Place of death groups sum to all people total",
              "English IMD deciles sum to England total",
              "Welsh IMD quintiles sum to Wales total",
              "English regions sum to England total",
              "English regions sum to England total",
              "Certification types sum to all types",
              "Place of occurrence sum to to all places",
              "Age groups sum to all ages",
              "Female and male counts sum to all people total",
              "NI, Scot, Eng, Wal and nonresidents sum to UK total",
              "Age groups sum to all ages total",
              "Female and male counts sum to all people total"),
    Match = c(
      subtotals_check(ew_registrations, "registrations", "region"),
      subtotals_check(ew_registrations, "registrations", "age"),
      subtotals_check(ew_registrations, "registrations", "sex"),
      subtotals_check(ew_registrations, "registrations", "place"),
      subtotals_check(ew_registrations, "registrations", "imd_eng"),
      subtotals_check(ew_registrations, "registrations", "imd_wales"),
      subtotals_check(ew_occurrences, "occurrences", "region"),
      subtotals_check(ew_occurrences, "occurrences", "age"),
      subtotals_check(ew_occurrences, "occurrences", "sex"),
      subtotals_check(ew_occurrences, "occurrences", "place"),
      subtotals_check(ew_occurrences, "occurrences", "imd_eng"),
      subtotals_check(ew_occurrences, "occurrences", "imd_wales"),
      subtotals_check(ew_registrations_by_cause, "registrations", "region"),
      subtotals_check(ew_reg_delays_cert_pod_area, "registrations", "region"),
      subtotals_check(
        ew_reg_delays_cert_pod_area, "registrations", "cert_type"),
      subtotals_check(ew_reg_delays_cert_pod_area, "registrations", "place"),
      subtotals_check(uk_excess_deaths, "registrations", "age"),
      subtotals_check(uk_excess_deaths, "registrations", "sex"),
      subtotals_check(uk_registrations, "registrations", "country"),
      subtotals_check(uk_registrations, "registrations", "age"),
      subtotals_check(uk_registrations, "registrations", "sex"))
  )

  sub_totals_df %>%
    dplyr::mutate(

      Match = kableExtra::cell_spec(
        Match,
        "html",
        background = ifelse(Match == TRUE, "lightgreen",
                            ifelse(Match == FALSE, "red", "yellow")))
    ) %>%
    kableExtra::kbl(format = "html", escape = FALSE) %>%
    kableExtra::kable_paper(c("striped", "hover"),
                            full_width = FALSE,
                            position = "left",
                            html_font = "Arial")
}


#' @title due_involve_check
#'
#' @description
#' Comparing the counts of deaths involving a condition with the counts of
#' deaths due to a condition for ONS weekly deaths release. Intended for use in
#' the production of a quality report.
#'
#' @details
#' This function compares the counts of deaths involving a condition with the
#' counts of deaths due to a condition. If the count of deaths involving a
#' condition is less than or equal to the count of deaths due to a condition
#' by week number and area of residence, then the row of interest is kept in
#' the final dataframe.
#'
#' @param data The R dataframe in the global environment containing the
#'             relevant data.
#'
#' @returns
#' A dataframe "due_involve_df" within the local function environment. Nothing
#' will be saved to the global environment by running the function alone.
#'
#' @examples
#' nrow(due_involve_check(ew_registrations_by_cause))
#'
#' @export
due_involve_check <- function(data) {

  due_involve_df <- data %>%
    janitor::clean_names("snake") %>%
    dplyr::filter(cause_of_death != "All causes") %>%
    dplyr::mutate(
      cause_of_death2 = dplyr::case_when(
        stringr::str_detect(cause_of_death, "J00 to J99") ~ "J00 to J99",
        stringr::str_detect(cause_of_death, "J09 to J18") ~ "J09 to J18",
        stringr::str_detect(cause_of_death, "U07.1, U07.2")
        ~ "U07.1, U07.2, U09.9, U10.9"),

      due = dplyr::case_when(
        substr(cause_of_death, 1, 10) == "Deaths due" ~ number_of_deaths,
        TRUE ~ 0),
      involve = dplyr::case_when(
        substr(cause_of_death, 1, 16) == "Deaths involving" ~ number_of_deaths,
        TRUE ~ 0)) %>%
    dplyr::group_by(week_number, week_ending, area_of_usual_residence,
                    cause_of_death2) %>%
    dplyr::summarise(
      due = max(due), involve = max(involve), .groups = "keep") %>%
    dplyr::filter(involve < due) %>%
    dplyr::rename("Week number" = week_number, "Week ending" = week_ending,
                  "Area of usual residence" = area_of_usual_residence,
                  "Cause of death" = cause_of_death2, "Deaths due to" = due,
                  "Deaths involving" = involve)
}

#' @title create_due_involve_table
#'
#' @description
#' Creates the dataframe with kable styling applied to show whether the number
#' of rows due to a cause is less than or equal to the number of rows involving
#' a cause
#'
#' @param data The R dataframe in the global environment containing the
#' relevant data.
#' @returns
#' A dataframe with a single row with an Outcome column of either TRUE, FALSE or
#' 'Check if table exists'
#' @export
create_due_involve_table <- function(cause_table) {

  table_exists <- exists(deparse(substitute(cause_table)))

  if (table_exists) {
    table_output_value <- nrow(due_involve_check(cause_table)) == 0
  } else {
    table_output_value <- "Check table exists."
  }

  output_df <- data.frame(
    Check = c("Number of deaths due to a cause are less than or equal to the
    number deaths involving the cause"),
    Outcome = c(table_output_value))
  output_df %>%
    dplyr::mutate(
      Outcome = kableExtra::cell_spec(
        Outcome,
        "html",
        background = ifelse(Outcome == TRUE, "lightgreen",
                            ifelse(Outcome == FALSE, "red", "yellow")))) %>%
    kableExtra::kbl(format = "html", escape = FALSE) %>%
    kableExtra::kable_paper(c("striped", "hover"),
                            full_width = FALSE,
                            position = "left",
                            html_font = "Arial")
}

#' @title create_due_involve_rows_table
#'
#' @description
#' Creates the dataframe with kable styling applied displaying the rows where
#' the due to number of deaths is greater than the involving number of deaths.
#' If there are no rows, nothing is returned.
#'
#' @param data The R dataframe in the global environment containing the
#' relevant data.
#' @returns
#' A dataframe containing the rows from the causes table where deaths due to a
#' cause exceed the deaths involving the cause.
#' @export
create_due_involve_rows_table <- function(cause_table) {

  table_exists <- exists(deparse(substitute(cause_table)))

  if (table_exists) {
    if (nrow(due_involve_check(cause_table)) > 0) {
      due_involve_check(cause_table) %>%
        dplyr::mutate(`Deaths due to` = kableExtra::cell_spec(
          `Deaths due to`, "html", background = "red"),
          `Deaths involving` = kableExtra::cell_spec(
            `Deaths involving`, "html", background = "red")) %>%
        kableExtra::kbl(format = "html", escape = FALSE) %>%
        kableExtra::kable_paper(c("striped", "hover"),
                                full_width = FALSE,
                                position = "left",
                                html_font = "Arial")
    }
  }
}

#' @title Creates table of quality checks on ASMRs with kableextra styling
#'
#' @description
#' Creates the dataframe with kable styling applied to check if ASMR is between
#' limits, UK value is between min and max country values and All people value
#' is between male and female
#'
#' @param data The R dataframe in the global environment containing asmr data
#' @returns
#' A dataframe with a three rows with an Outcome column of either TRUE, FALSE,
#' Check or 'Check if table exists' with styling applied
#' @export
create_asmr_check_table <- function(asmr_table) {

  table_exists <- exists(deparse(substitute(asmr_table)))

  if (table_exists) {
    table_output_values <- c(
      ifelse(
        nrow(get_rows_asmr_outside_limits(asmr_table)) == 0,
        "TRUE",
        "FALSE"),
      ifelse(
        nrow(get_rows_uk_asmr_out_ctrys(asmr_table)) == 0,
        "TRUE",
        "FALSE - for info"),
      ifelse(
        nrow(get_rows_asmr_all_ppl_out_m_f(asmr_table)) == 0,
        "TRUE",
        "FALSE - for info")
      )
  } else {
    table_output_values <- rep("Check table exists.", 3)
  }

  output_df <- data.frame(
    Check = c(
      "ASMR value between lower and upper limit",
      "UK ASMR is between min and max of individual countries",
      "All people ASMR is between male and female ASMRs"
              ),
    Outcome = table_output_values)
  output_df %>%
    dplyr::mutate(
      Outcome = kableExtra::cell_spec(
        Outcome,
        "html",
        background = ifelse(Outcome == "TRUE", "lightgreen",
                            ifelse(Outcome == "FALSE", "red", "yellow")))) %>%
    kableExtra::kbl(format = "html", escape = "FALSE") %>%
    kableExtra::kable_paper(c("striped", "hover"),
                            full_width = FALSE,
                            position = "left",
                            html_font = "Arial")


}

#' @title Creates table where 'All people' asmr is unusually high or low
#' @description Creates the df where the 'All people' ASMR is outside the male
#' and female ASMRs. kableextra styling is applied ready to put in an Rmd.
#' @details Highlighted yellow as it is possible for this to happen without
#' being an error.
#' @param data The R dataframe in the global environment containing asmr data
#' @returns
#' A dataframe with kableExtra styling applied
#' @export
create_asmr_all_ppl_out_table <- function(asmr_table) {

  table_exists <- exists(deparse(substitute(asmr_table)))

  if (table_exists) {
    if (nrow(get_rows_asmr_all_ppl_out_m_f(asmr_table)) > 0) {
      get_rows_asmr_all_ppl_out_m_f(asmr_table) %>%
        dplyr::mutate(
          `All people` = kableExtra::cell_spec(
          `All people`, "html", background = "yellow")) %>%
        kableExtra::kbl(format = "html", escape = FALSE) %>%
        kableExtra::kable_paper(c("striped", "hover"),
                                full_width = FALSE,
                                position = "left",
                                html_font = "Arial")
    }
  }
}

#' @title Create table where UK asmr is unusally high or low
#' @description Creates the df where the UK ASMR is outside the min and max of
#' the individual country asmrs. kableextra styling is applied ready to put in
#' an Rmd.
#' @details Highlighted yellow as it is possible for this to happen without
#' being an error.
#' @param data The R dataframe in the global environment containing asmr data
#' @returns
#' A dataframe with kableExtra styling applied
#' @export
create_asmr_uk_out_table <- function(asmr_table) {

  table_exists <- exists(deparse(substitute(asmr_table)))

  if (table_exists) {
    if (nrow(get_rows_uk_asmr_out_ctrys(asmr_table)) > 0) {
      get_rows_uk_asmr_out_ctrys(asmr_table) %>%
        rename(
          `Min individual country ASMR` = min_ctry_asmr,
          `Max individual country ASMR` = max_ctry_asmr,
        ) %>%
        dplyr::mutate(
          `ASMR per 100,000` = kableExtra::cell_spec(
            `ASMR per 100,000`, "html", background = "yellow")) %>%
        kableExtra::kbl(format = "html", escape = FALSE) %>%
        kableExtra::kable_paper(c("striped", "hover"),
                                full_width = FALSE,
                                position = "left",
                                html_font = "Arial")
    }
  }
}


#' @title Create table where asmr is ouside upper and lower bound values
#' @description Creates the df where the ASMR is outside the upper and lower
#' limits. kableextra styling is applied ready to put in an Rmd.
#' @param data The R dataframe in the global environment containing asmr data
#' @returns
#' A dataframe with kableExtra styling applied
#' @export
create_asmr_out_lims_table <- function(asmr_table) {

  table_exists <- exists(deparse(substitute(asmr_table)))

  if (table_exists) {
    if (nrow(get_rows_asmr_outside_limits(asmr_table)) > 0) {
      get_rows_asmr_outside_limits(asmr_table) %>%
        dplyr::mutate(
          `ASMR per 100,000` = kableExtra::cell_spec(
            `ASMR per 100,000`, "html", background = "red")) %>%
        kableExtra::kbl(format = "html", escape = FALSE) %>%
        kableExtra::kable_paper(c("striped", "hover"),
                                full_width = FALSE,
                                position = "left",
                                html_font = "Arial")
    }
  }
}

#' @title display_asmr_warning_text
#' @description Creates the text to display when there are rows in the asmr
#' table which fail the checks.
#' @param data The R dataframe in the global environment containing asmr data
#' @returns char with knitr styling applied.
#' @export
display_asmr_warning_text <- function(asmr_table) {

  if (nrow(get_rows_asmr_outside_limits(asmr_table)) > 0 ||
     nrow(get_rows_uk_asmr_out_ctrys(asmr_table)) > 0 ||
     nrow(get_rows_asmr_all_ppl_out_m_f(asmr_table)) > 0) {
    asmr_warning_text <- paste0(
      "If any of the checks fail, the rows that cause the failure will be ",
      "displayed below.\n\n\nChecks on the UK ASMR (between min and max of the",
      " individual countries) and All people ASMR (between male and female) ",
      "are for awareness only and not caused by an error. It may seem ",
      "counterintuitive but these can happen due to the age distribution and ",
      "ESP weightings."
    )

    knitr::asis_output(asmr_warning_text)
  }
}


#' @title Gets rows where asmr value is not between lower and upper limits
#'
#' @description Creates a df of rows where the asmr table fails the check of
#' asmr being between lower and upper limits
#'
#' @param asmr_df dataframe with cols `ASMR per 100,000`,
#' `Lower confidence limit` and `Upper confidence limit`
#' @return dataframe
#' @export
#'
get_rows_asmr_outside_limits <- function(asmr_df) {
  asmr_df %>%
    dplyr::filter(
      as.numeric(`ASMR per 100,000`) < as.numeric(`Lower confidence limit`) |
        as.numeric(`ASMR per 100,000`) > as.numeric(`Upper confidence limit`)
    )
}

#' @title Gets rows where UK ASMR is outside individual countries ASMRs
#'
#' @description Creates a df of rows where the asmr table fails the check of
#' the UK asmr being between min and max of individual countries of the UK
#' @details It is possible for this to happen without error due to the weighting
#' of populations by sex and ESP.
#' @param asmr_df dataframe with cols `ASMR per 100,000`, `Country`, `Sex`,
#' `Week number`
#' @return dataframe
#' @export
#'
get_rows_uk_asmr_out_ctrys <- function(asmr_df) {
  asmr_df <- dplyr::mutate(
    asmr_df,
    `ASMR per 100,000` = as.numeric(`ASMR per 100,000`)
  )

  asmr_min_max <- asmr_df %>%
    dplyr::filter(Country != "UK") %>%
    dplyr::group_by(`Week number`, `Sex`) %>%
    dplyr::summarise(
      min_ctry_asmr = min(`ASMR per 100,000`),
      max_ctry_asmr = max(`ASMR per 100,000`),
      .groups = "drop"
    )

  asmr_df %>%
    dplyr::filter(Country == "UK") %>%
    dplyr::left_join(asmr_min_max, by = c("Week number", "Sex")) %>%
    dplyr::filter(
      `ASMR per 100,000` < min_ctry_asmr | `ASMR per 100,000` > max_ctry_asmr)
}

#' @title Gets rows where 'All people' ASMR is outside male and female ASMRs
#'
#' @description Creates a df of rows where the all people asmr table fails the
#' check of being between the male and female ASMRS
#' @details It is possible for this to happen without error due to the weighting
#' of populations by sex and ESP.
#' @param asmr_df dataframe with cols `ASMR per 100,000`, `Country`, `Sex`,
#' `Week number`
#' @return dataframe
#' @export
#'
get_rows_asmr_all_ppl_out_m_f <- function(asmr_df) {
  asmr_df <- dplyr::mutate(
    asmr_df,
    `ASMR per 100,000` = as.numeric(`ASMR per 100,000`)
  )
  asmr_df %>%
    tidyr::pivot_wider(
      id_cols = c("Week number", "Country"),
      names_from = Sex,
      values_from = `ASMR per 100,000`) %>%
    dplyr::filter(`All people` < pmin(Female, Male) |
                  `All people` > pmax(Female, Male))
}


#' Create QA table comparing counts between table 1 and table 3 for England,
#' Wales and Non-residents, England and Wales
#'
#' @param table1 data frame containing table 1 England and Wales death
#' registrations data
#' @param table2 data frame containing table 3 England and Wales death
#' registrations data by cause of death
#'
#' @return a QA table comparing table 1 and table 3 counts for England,
#' Wales and Non-residents, England and Wales.
#' @export
#'
qa_table_3_counts <- function(table1, table2) {

  tbl_1_reg <- table1 %>%
    dplyr::filter(Sex == "All people",
                  `Age group (years)` == "All ages",
                  `Place of occurrence` == "All places",
                  `IMD quantile group` == "All groups") %>%
    dplyr::select(`Week number`,
                  `Area of usual residence`,
                  `Number of deaths`)

  tbl_3_reg <- table2 %>%
    dplyr::filter(`Cause of death` == "All causes") %>%
    dplyr::select(`Week number`,
                  `Area of usual residence`,
                  `Number of deaths`)

  areas <- c("England, Wales and non-residents", "England", "Wales")

  tbl_3_results <- check_area_outputs_match(tbl_1_reg, tbl_3_reg, areas)

  tbl_3_results <- format_qa_table(tbl_3_results)

  knitr::kable(tbl_3_results, format = "simple")
}

#' Create QA table comparing counts between table 1 and table 4 for England,
#' Wales and Non-residents.
#'
#' @param table1 data frame containing table 1 England and Wales death
#' registrations data
#' @param table2 data frame containing table 4 England and Wales death
#' registration delays data
#'
#' @return a QA table comparing table 1 and table 4 counts for England,
#' Wales and Non-residents.
#' @export
#'
qa_table_4_counts <- function(table1, table2) {
  tbl_1_delay <- table1 %>%
    dplyr::filter(
      `Area of usual residence` == "England, Wales and non-residents",
      Sex == "All people",
      `Age group (years)` == "All ages",
      `Place of occurrence` == "All places") %>%
    dplyr::select(`Week number`,
                  "Deaths registered in week" = `Number of deaths`)

  tbl_4_delay <- table2 %>%
    dplyr::filter(
      `Area of usual residence` == "England, Wales and non-residents",
      `Place of occurrence` == "All places",
      `Certification type` == "All deaths") %>%
    dplyr::select(`Week number`,
                  `Deaths registered in week`)

  tbl_4_results <- data.frame(
    `Level Check` = c("Deaths by week"),
    match = all.equal(tbl_1_delay$`Deaths registered in week`,
                      tbl_4_delay$`Deaths registered in week`),
    check.names = FALSE
  ) %>%
    format_qa_table()

  knitr::kable(tbl_4_results, format = "simple")
}

#' Create QA table comparing counts between table 1 and table 5 for England,
#' Wales and Non-residents, England and Wales
#'
#' @param table1 data frame containing table 1 England and Wales death
#' registrations data
#' @param table2 data frame containing table 5 England and Wales excess deaths
#' data
#'
#' @return a QA table comparing table 1 and table 5 counts for England,
#' Wales and Non-residents, England and Wales.
#' @export
#'
qa_table_5_counts <- function(table1, table2) {

  lvl <- create_factor_levels()

  tbl_1_occ <- table1 %>%
    dplyr::select(-`Week ending`) %>%
    dplyr::filter(`Place of occurrence` == "All places",
                  `IMD quantile group` == "All groups",
                  `Area of usual residence` %in%
                    c("England, Wales and non-residents", "England", "Wales"),
                  !(`Area of usual residence` %in% c("England", "Wales") &
                      `Age group (years)` != "All ages")) %>%
    dplyr::mutate(`Age group (years)` = dplyr::case_when(
      `Age group (years)` %in%
        c("Under 1", "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",
          "25 to 29") ~ "0 to 29",
      `Age group (years)` %in% c("30 to 34", "35 to 39", "40 to 44") ~
        "30 to 44",
      `Age group (years)` %in% c("90 to 94", "95 to 99", "100 and over") ~
        "90 and over",
      .default = `Age group (years)`)
    ) %>%
    dplyr::group_by(`Week number`,
                    `Area of usual residence`,
                    Sex,
                    `Age group (years)`) %>%
    dplyr::summarise_if(is.integer, sum) %>%
    dplyr::mutate(
      `Age group (years)` = factor(`Age group (years)`,
                                   levels = lvl$agegrp_excess),
      `Area of usual residence` = factor(`Area of usual residence`,
                                         levels = lvl$country_region)) %>%
    dplyr::arrange(dplyr::desc(`Week number`),
                   `Area of usual residence`,
                   Sex,
                   `Age group (years)`,
                   `Number of deaths`)

  tbl_5_occ <- table2 %>%
    dplyr::rename(`Area of usual residence` = `Country`) %>%
    dplyr::filter(`Area of usual residence` %in%
                    c("England, Wales and non-residents",
                      "England",
                      "Wales")) %>%
    dplyr::select(`Week number`,
                  `Area of usual residence`,
                  Sex,
                  `Age group (years)`,
                  `Number of deaths`) %>%
    dplyr::arrange(desc(`Week number`),
                   `Area of usual residence`,
                   Sex,
                   `Age group (years)`,
                   `Number of deaths`)

  areas <- c("England, Wales and non-residents", "England", "Wales")

  tbl_5_results <- check_area_outputs_match(tbl_1_occ, tbl_5_occ, areas) %>%
    format_qa_table()

  knitr::kable(tbl_5_results, format = "simple")

}

#' Create QA table comparing counts between table 1 and table 6 for England,
#' Wales and Non-residents, England and Wales
#'
#' @param table1 data frame containing table 1 England and Wales death
#' registrations data
#' @param table2 data frame containing table 6 UK death registrations data
#'
#' @return a QA table comparing table 1 and table 6 counts for England,
#' Wales and Non-residents, England and Wales.
#' @export
#'
qa_table_6_counts <- function(table1, table2) {

  if (!exists(deparse(substitute(table2)))) {

    tbl_6_results <- data.frame(
      "Area Level Checked" = c("England, Wales and non-residents", "England",
                               "Wales", "Other areas"),
      `match` = rep("UK data not available. Rerun on Friday for UK QA table")
    ) %>%
      format_qa_table()

  } else {
    lvl <- create_factor_levels()

    tbl_1_sex_age <- table1 %>%
      dplyr::filter(`Place of occurrence` == "All places",
                    `IMD quantile group` == "All groups",
                    `Area of usual residence` %in%
                      c("England, Wales and non-residents",
                        "England",
                        "Wales")) %>%
      dplyr::select(Week = `Week number`,
                    Country = `Area of usual residence`,
                    Age = `Age group (years)`,
                    Sex,
                    `Number of deaths`) %>%
      dplyr::mutate(Age = dplyr::case_when(
        Age %in% c("1 to 4", "5 to 9", "10 to 14") ~ "1 to 14",
        Age %in% c("15 to 19", "20 to 24", "25 to 29", "30 to 34",
                   "35 to 39", "40 to 44") ~ "15 to 44",
        Age %in% c("45 to 49", "50 to 54", "55 to 59", "60 to 64") ~ "45 to 64",
        Age %in% c("65 to 69", "70 to 74") ~ "65 to 74",
        Age %in% c("75 to 79", "80 to 84") ~ "75 to 84",
        Age %in% c("85 to 89", "90 to 94", "95 to 99", "100 and over")
        ~ "85 and over",
        Age == "All ages" ~ "All ages",
        Age == "Under 1" ~ "Under 1",
        .default = NULL),
      ) %>%
      dplyr::group_by(Week,
                      Country,
                      Age,
                      Sex) %>%
      dplyr::summarise_if(is.integer, sum) %>%
      dplyr::mutate(Age = factor(Age, levels = lvl$agegrp_wide),
                    Country = factor(Country, levels = lvl$country_region)) %>%
      dplyr::arrange(desc(Week),
                     Country,
                     Age,
                     Sex,
                     `Number of deaths`)

    tbl_6_sex_age <- table2 %>%
      dplyr::filter(Country %in%
                      c("England, Wales and non-residents",
                        "England",
                        "Wales")) %>%
      dplyr::arrange(dplyr::desc(`Week number`),
                     Country,
                     `Age group (years)`,
                     Sex,
                     `Number of deaths`)

    areas <- c("England, Wales and non-residents", "England", "Wales")

    tbl_6_results <- check_area_outputs_match(tbl_1_sex_age,
                                              tbl_6_sex_age,
                                              areas,
                                              area_column = "Country") %>%
      format_qa_table()
  }

  knitr::kable(tbl_6_results, format = "simple")
}

#' Compare two tables to verify they contain the same results
#'
#' @param tbl_baseline dataframe containing England and Wales death data for
#' comparison
#' @param tbl_to_check dataframe containing England and Wales death data to be
#' checked
#' @param areas vector of strings for each area to be specifically checked
#' @param area_column string, area column name in tables
#' @param death_column string, amount of deaths column name in tables
#'
#' @return dataframe containing the results from checking the table outputs
#' @export
check_area_outputs_match <- function(
    tbl_baseline,
    tbl_to_check,
    areas,
    area_column = "Area of usual residence",
    death_column = "Number of deaths") {

  results <- data.frame()

  for (curr_area in areas) {
    curr_base_area <- tbl_baseline %>%
      dplyr::filter(area_column == curr_area) %>%
      dplyr::pull(death_column)

    curr_check_area <- tbl_to_check %>%
      dplyr::filter(area_column == curr_area) %>%
      dplyr::pull(death_column)

    curr_match <- all.equal(curr_base_area,
                            curr_check_area)

    results <- rbind(results, list(curr_area, curr_match))
  }

  # check whether any remaining areas match
  curr_base_other <- tbl_baseline %>%
    dplyr::filter(!(area_column %in% areas)) %>%
    dplyr::pull(death_column)

  curr_check_other <- tbl_to_check %>%
    dplyr::filter(!(area_column %in% areas)) %>%
    dplyr::pull(death_column)

  curr_match <- all.equal(curr_base_other,
                          curr_check_other)

  results <- rbind(results, list("Other areas", curr_match))

  colnames(results) <- c("Area Level Checked", "match")

  results
}

#' Function to add background colour to matching column
#'
#' @param df QA data frame - containing columns Area level checked and match
#'
#' @return data frame with coloured background to indicate if QA is matching,
#' not matching or needs checking
#' @export
#'
format_qa_table <- function(df) {
  df <- df %>%
    dplyr::mutate(match = kableExtra::cell_spec(
      match,
      "html",
      background = ifelse(match == TRUE, "lightgreen",
                          ifelse(match == FALSE, "red", "yellow"))))
}
