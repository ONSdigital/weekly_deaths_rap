#' Process weekly deaths for England and Wales
#'
#' @description Re-codes age group, sex, place of death, region
#' and country columns. Some columns have also been renamed. This data has
#' validated previously so the values will be as expected. Drops records
#' with `dor` before the max date of registration.
#'
#' @details Age groups are processed to publishing format. Sex is re-coded
#' from 1 to Male and 2 to Female. Missing country is re-coded to "Nonresident".
#' "Other communal establishment" in place of death is re-coded to "Elsewhere".
#' reg_stat_dod is renamed to dod.
#'
#' @param data Unprocessed EW weekly deaths dataframe.
#' @param nspl NSPL lookup dataframe.
#' @param max_reg_date Date of cut off for the week's registrations, as
#' character.
#'
#' @return Processed EW weekly deaths dataframe.
#' @export
#'
process_ew_weekly_deaths <- function(data, nspl, max_reg_date) {

  data %>%
    dplyr::filter(dor <= as.numeric(max_reg_date)) %>%
    process_agegrp(agegroup_wide) %>%
    process_agegrp(agegroup_2) %>%
    change_max_agegrp() %>%
    flag_cert_type_groups(certtype) %>%
    flag_cert_type_agg(cert_type) %>%
    dplyr::mutate(
      dod = reg_stat_dod,
      sex = dplyr::if_else(sex == 1, "Male", "Female"),
      agegrp_wide = agegroup_wide,
      agegrp_5yr = agegroup_2,
      country = dplyr::if_else(is.na(ctrynm), "Nonresident", ctrynm),
      place_of_death = dplyr::if_else(
        pod == "Other communal establishment",
        "Elsewhere",
        pod),
      .before = dor_week) %>%
    add_lsoa(nspl) %>%
    dplyr::select(-c(agegroup_wide, agegroup_2, pod, ctrynm, reg_stat_dod,
                     dor_week))
}


#' Process deaths for England and Wales from annual file
#'
#' @param data data frame containing annual data for deaths registered in
#' England and Wales, compatibile with data from 2001
#' @param nspl data frame containing pcd column with postcodes and gor column
#' with codes for regions
#'
#' @return Processed EW weekly deaths data frame - similar to one produced using
#' provisional data files.
#' @export
#'
process_ew_annual <- function(data, nspl) {
  data %>%
    process_agegrp(agegroup_2) %>%
    change_max_agegrp() %>%
    flag_cert_type_groups(certtype) %>%
    flag_cert_type_agg(cert_type) %>%
    dplyr::mutate(
      sex = dplyr::if_else(sex == 1, "Male", "Female"),
      agegrp_5yr = agegroup_2,
      pcdr = gsub(" ", "", pcdr)) %>%
    flag_agegrp_wide(ageinyrs) %>%
    flag_place_of_death_annual %>%
    flag_country_region(nspl) %>%
    dplyr::select(-c(agegroup_2, ctryir))
}

#' Process weekly deaths for Scotland
#'
#' @description Re-codes age group and sex columns. Some columns have also been
#' renamed. Country column added. Drops records with `dor` after the max date
#' of registration.
#'
#' @details Age groups are processed to publishing format. Sex is re-coded
#' from 1 to Male and 2 to Female.
#' If scope  = "EW" then the previous week is used for Scotland data so
#' max_reg_date is one week before
#'
#' @param data Unprocessed weekly deaths dataframe for Scotland.
#' @param config list containing week (analytical week), year (analytical year)
#' and scope (string of "UK" or "EW")
#'
#' @return Processed weekly deaths dataframe for Scotland.
#' @export
process_scot_weekly_deaths <- function(data, config) {

  max_reg_date <- derive_max_reg_date(config, is_scot = TRUE)

  if (config$scope == "EW") {
    max_reg_date <- calc_date_one_week_before(max_reg_date)
  }

  data %>%
    dplyr::filter(dor <= as.numeric(max_reg_date))  %>%
    process_agegrp(ageband_wide) %>%
    dplyr::mutate(
      sex = dplyr::if_else(sex == 1, "Male", "Female"),
      country = "Scotland") %>%
    dplyr::rename(
      agegrp_wide = ageband_wide)
}


#' Process weekly deaths for Northern Ireland
#'
#' @description Re-codes age group and sex columns. Some columns have also been
#' renamed. Country column added. Drops records with `dor` after the max date
#' of registration.
#'
#' @details Age groups are processed to publishing format. Sex is re-coded
#' from 1 to Male and 2 to Female.
#' If scope  = "EW" then  the previous week is used for NI data so max_reg_date
#' is one week before.
#'
#' @param data Unprocessed weekly deaths dataframe for NI.
#' @param max_reg_date Date of cut off for the week's registrations, as
#' character
#' @param scope string of "UK" or "EW"
#'
#' @return Processed weekly deaths dataframe for NI.
#' @export
#'
process_ni_weekly_deaths <- function(data, max_reg_date, scope) {
  if (scope == "EW") {
    max_reg_date <- calc_date_one_week_before(max_reg_date)
  }
  data %>%
    dplyr::filter(dor <= as.numeric(max_reg_date)) %>%
    process_agegrp(age_wide) %>%
    dplyr::mutate(
      sex = dplyr::if_else(sex == 1, "Male", "Female"),
      country = "Northern Ireland") %>%
    dplyr::rename(
      agegrp_wide = age_wide)
}


#' Process age groups
#'
#' @description Re-codes a column with publishing ready age group values.
#'
#' @details Changes "+" to "and over", "-" to " to ", "<1" to "Under 1" and
#' removes any leading 0s.
#'
#' @param data Data frame of unprocessed deaths data.
#' @param column Unquoted name of column to process.
#'
#' @return Data frame with extra column.
#' @export
#'
process_agegrp <- function(data, column) {

  dplyr::mutate(
    data,
    {{ column }} := stringr::str_replace_all(
      {{ column }},
      c("[+]" = " and over",
        "[-]" = " to ",
        "<1" = "Under 1",
        "01" = "1",
        "04" = "4",
        "05" = "5",
        "09" = "9")))
}


#' Changes the maximum aggregations of the 5-year age bands to 100 and over
#'
#' @param data Dataframe with processed `agegroup_2` column.
#'
#' @return Dataframe with the changed `agegroup_2`.
#' @export
#'
change_max_agegrp <- function(data) {

  dplyr::mutate(
    data,
    agegroup_2 = dplyr::case_when(
      ageinyrs >= 90 & ageinyrs < 95 ~ "90 to 94",
      ageinyrs >= 95 & ageinyrs < 100 ~ "95 to 99",
      ageinyrs >= 100 ~ "100 and over",
      .default = agegroup_2
    )
  )
}

#' Reverse the maximum aggregations of the 5-year age bands to match
#' European Standard Population (ESP) age bands
#'
#' @description ESP top age group is "90 and over" so we must align for excess
#' deaths. Also renames `agegrp_5yr` to `agegroup_2`.
#'
#' @param data Dataframe with processed `agegrp_5yr` column.
#'
#' @return Dataframe with `agegroup_2` which maxes out at `90 and over`.
#' @export
#'
reverse_max_agegrp <- function(data) {
  data %>%
    dplyr::mutate(
      agegroup_2 = dplyr::if_else(
        agegrp_5yr %in% c("90 to 94", "95 to 99", "100 and over"),
        "90 and over", agegrp_5yr)) %>%
    dplyr::select(-agegrp_5yr)
}


#' Flag cause of death for weekly deaths publication.
#'
#' @description Uses ICD10 codes to create new columns with boolean entries
#' (TRUE or FALSE) for whether deaths are due to or involving causes (diseases
#' of the respiratory system, influenza or pneumonia, and COVID-19). Due to is
#' is TRUE if the code is present in the "fic10und" column, involving is TRUE
#' if the code is in any of the fic10 columns.
#'
#' @details Respiratory system codes are J00 to J99 (inc. 4-digit codes).
#' Influenza or pneumonia codes are J09 to J18 (inc. 4-digit codes).
#' COVID-19 codes are U071, U072, U099 and U109.
#'
#' @param data Weekly deaths data frame.
#'
#' @return Data frame with extra columns.
#' @export
#'
flag_weekly_causes <- function(data) {

  codes_resp <- sprintf("J%02d", 0:99)
  codes_infl <- sprintf("J%02d", 9:18)
  codes_covid <- c("U071", "U072", "U099", "U109")

  data %>%
    flag_cause(resp_inv, codes_resp) %>%
    flag_cause(resp_due, codes_resp, due_to = TRUE) %>%
    flag_cause(infl_inv, codes_infl) %>%
    flag_cause(infl_due, codes_infl, due_to = TRUE) %>%
    flag_cause(covid_inv, codes_covid) %>%
    flag_cause(covid_due, codes_covid, due_to = TRUE)
}


#' Flag cause of death
#'
#' @description Uses vector of ICD10 codes to search columns for matches. If
#' due_to is set to FALSE, it will search columns beginning with "fic"
#' (including underlying cause, "fic10und"), and if due_to is set to TRUE,
#' it will only look at "fic10und". A new column is created with TRUE or FALSE
#' as to whether there are any matches.
#'
#' @details For non-COVID-19 codes, 4-digit codes will match to their 3-digit
#' header: e.g., "J182" would flag in "J18". COVID-19 codes require a 4-digit
#' code match.
#'
#' @param data Deaths data frame.
#' @param new_col Unquoted name of new column to create.
#' @param codes Vector of strings of ICD10 codes.
#' @param due_to Boolean of whether the flag is for deaths due to cause codes
#' (TRUE) or involving cause codes (FALSE). Default is FALSE. Error if wrong
#' arg given.
#'
#' @return Data frame with extra column.
#' @export
#'
flag_cause <- function(data, new_col, codes, due_to = FALSE) {

  if (isTRUE(due_to)) {
    col <- "fic10und"
  } else if (isFALSE(due_to)) {
    col <- "fic"
  } else {
    stop("due_to argument must be TRUE or FALSE")
  }

  dplyr::mutate(
    data,
    {{ new_col }} := dplyr::if_any(
      tidyr::starts_with(col),
      ~ stringr::str_detect(., paste(codes, collapse = "|"))),
    {{ new_col }} := ifelse(is.na({{ new_col }}), "FALSE", {{ new_col }}))
}

#' Flag age group (wide)
#'
#' @param data Data frame with column for age in years.
#' @param age_col Unquoted name of column with numeric age (in years).
#'
#' @return Data frame with added agegrp_wide column.
#' @export
#'
flag_agegrp_wide <- function(data, age_col) {
  data %>%
    dplyr::mutate(
      agegrp_wide = dplyr::case_match(
        {{ age_col }},
        0 ~ "Under 1",
        1:14 ~ "1 to 14",
        15:44 ~ "15 to 44",
        45:64 ~ "45 to 64",
        65:74 ~ "65 to 74",
        75:84 ~ "75 to 84",
        85:200 ~ "85 and over",
        .default = NA_character_))
}

#' Flag age group (for excess publication)
#'
#' @param data Data frame with column for age in years.
#' @param age_col Unquoted name of column with numeric age (in years).
#'
#' @return Data frame with added agegrp_wide column.
#' @export
#'
flag_agegrp_excess <- function(data, age_col) {
  dplyr::mutate(
    data,
    agegrp_excess = dplyr::case_match(
      {{ age_col }},
      0:29 ~ "0 to 29",
      30:44 ~ "30 to 44",
      45:49 ~ "45 to 49",
      50:54 ~ "50 to 54",
      55:59 ~ "55 to 59",
      60:64 ~ "60 to 64",
      65:69 ~ "65 to 69",
      70:74 ~ "70 to 74",
      75:79 ~ "75 to 79",
      80:84 ~ "80 to 84",
      85:89 ~ "85 to 89",
      90:200 ~ "90 and over",
      .default = NA_character_))
}

#' Process previous years - update when more functions are added
#'
#' @description Processes previous years of data to compute 5 year averages.
#'
#' @details Adds week number, week ending and year for dor, filters data to
#' required years for 5 year average, adds column for wide age group, gives sex
#' column as factor, adds respiratory/covid columns, adds country column as char
#' and adds place_of_death_other column.
#'
#' @param data Data frame of finalised deaths data.
#' @param years vector of required years
#'
#' @return Processed data frame with additional columns.
#' @export
#'
process_prev_years <- function(data, years) {

  data %>%
    derive_years_week_numbers %>%
    dplyr::filter(year %in% years) %>%
    flag_agegrp_wide(ageinyrs) %>%
    dplyr::mutate(sex = dplyr::if_else(sex == 1, "Male", "Female")) %>%
    flag_country_ew() %>%
    flag_place_of_death_other("finalised")
}

#' Flag country (England and Wales)
#'
#' @description Uses `ctryr` variable containing country classification codes
#' to flag entries with "England" (921), "Wales" (924) or "Nonresident"
#' (all others).
#' @param data Data frame with ctryr column.
#'
#' @return Data frame with added country column
#' @export
#'
flag_country_ew <- function(data) {

  dplyr::mutate(
    data,
    country = dplyr::case_match(
      ctryr,
      924 ~ "Wales",
      921 ~ "England",
      .default = "Nonresident")
  )
}

#' Flag place of death for UK excess mortality
#'
#' @description Creates place_of_death_other column with values: "Home",
#' "Hospital", "Care home", and "Other". If data is "provisional",
#' `place_of_death` column is used to derive  `place_of_death_other`. If data is
#' "finalised", `place_of_death_other` is derived from `cestrss`, `nhsind`, and
#' `esttyped` as there is no `place_of_death` column.
#'
#' @details `cestrss` has values of "H" for home, otherwise numeric to denote
#' communal establishment. `place_of_death` is dropped for the "provisional"
#' processing.
#'
#' @param data Data frame or England + Wales data.
#' @param data_type String of "finalised" or "provisional", describing what type
#' of data is supplied. An error will be given if the input does not match one
#' of these strings.
#'
#' @return Data frame with place_of_death_other column.
#' @export
#'
flag_place_of_death_other <- function(data, data_type) {

  if (data_type == "provisional") {
    data %>%
      dplyr::mutate(place_of_death_other = dplyr::if_else(
        place_of_death %in% c("Hospice", "Elsewhere"),
        "Other", place_of_death)) %>%
      dplyr::select(-place_of_death)
  } else if (data_type == "finalised") {
    data %>%
      dplyr::mutate(
        place_of_death_other = dplyr::case_when(
          cestrss == "H" ~ "Home",
          esttyped %in% c(1, 3, 18, 99) & nhsind == 1 ~ "Hospital",
          esttyped %in% c(1, 18, 19) & nhsind == 2 ~ "Hospital",
          (esttyped %in% c(2, 4, 7, 10, 21) & nhsind == 1) |
            (esttyped %in% c(3, 4, 7, 10, 14, 20, 22, 32, 33) & nhsind == 2)
          ~ "Care home",
          .default = "Other")) %>%
      dplyr::select(-c(cestrss, esttyped, nhsind))
  } else {
    stop('data_type argument must be "finalised" or "provisional"')
  }
}


#' Convert place of deaths into names for annual files
#'
#' @param data annual deaths data frame with following columns: cestrss,
#' esttyped, nhsind
#'
#' @return data frame with new place_of_death column, specified as one of: Home,
#' Hospital, Care home, Hospice or Elsewhere; Columns cestrss, esttyped, nhsind
#' have been removed
#' @export
#'
flag_place_of_death_annual <- function(data) {
  data %>%
    dplyr::mutate(
      place_of_death = dplyr::case_when(
        cestrss == "H" ~ "Home",
        esttyped %in% c(1, 3, 18, 99) & nhsind == 1 ~ "Hospital",
        esttyped %in% c(1, 18, 19) & nhsind == 2 ~ "Hospital",
        (esttyped %in% c(2, 4, 7, 10, 21) & nhsind == 1) |
          (esttyped %in% c(3, 4, 7, 10, 14, 20, 22, 32, 33) & nhsind == 2)
        ~ "Care home",
        esttyped == 83 & nhsind %in% c(1, 2) ~ "Hospice",
        .default = "Elsewhere")) %>%
    dplyr::select(-c(cestrss, esttyped, nhsind))
}

#' Convert postcode into country and region information
#'
#' @param data data frame with pcdr column specifying postcode of residence
#' @param nspl data frame with pcd column with postcodes and gor column with
#' codes for regions
#'
#' @return original data frame with additional region and country columns, but
#' without pcdr column
#' @export
#'
flag_country_region <- function(data, nspl) {

  nspl <- nspl %>%
    dplyr::mutate(pcd = gsub(" ", "", pcd))

  data %>%
    dplyr::left_join(nspl, by = dplyr::join_by(pcdr == pcd)) %>%
    dplyr::mutate(
      region = dplyr::case_when(
        gor == "E12000001" ~ "North East",
        gor == "E12000002" ~ "North West",
        gor == "E12000003" ~ "Yorkshire and The Humber",
        gor == "E12000004" ~ "East Midlands",
        gor == "E12000005" ~ "West Midlands",
        gor == "E12000006" ~ "East of England",
        gor == "E12000007" ~ "London",
        gor == "E12000008" ~ "South East",
        gor == "E12000009" ~ "South West",
        gor == "W99999999" ~ "Wales",
        .default = "Nonresident"),
      country = dplyr::case_when(
        grepl("E", gor) ~ "England",
        grepl("W", gor) ~ "Wales",
        .default = "Nonresident")) %>%
    dplyr::select(-c(gor, pcdr))
}

#' Derive certification type
#'
#' @param data Annual deaths data frame with following columns: `certtype`.
#' @param cert_column Unquoted string of the `cert_type` column's name.
#'
#' @return Data frame with new `cert_type` column, specified as one of:
#' Certified by doctor, Certified by coroner with inquest,
#' Certified by coroner with no inquest, Uncertified/Unknown or
#' Certified by medical examiner.
#' @export
#'
flag_cert_type_groups <- function(data, cert_column) {
  dplyr::mutate(
    data,
    cert_type = dplyr::case_match(
      {{ cert_column }},
      c(1, 2, 7) ~ "Certified by doctor",
      c(3, 4, 8) ~ "Certified by coroner with inquest",
      c(5, 13, 14) ~ "Certified by coroner with no inquest",
      c(6, 9, NA) ~ "Unknown/Uncertified",
      c(10, 11, 12) ~ "Certified by medical examiner",
      .default = NA_character_))
}

#' Creates aggregated certification type
#' @details Converts `cert_type` to add an aggregated `cert_type` for
#' 'certified by coroner'.
#' @param data Data frame with following columns: `cert_type`.
#' @param cert_column Unquoted string of the `cert_type` column's name.
#'
#' @return Data frame with new column `cert_type_agg`.
#' @export
#'
flag_cert_type_agg <- function(data, cert_column) {
  dplyr::mutate(
    data,
    cert_type_agg = dplyr::case_match(
      {{ cert_column }},
      c("Certified by coroner with inquest",
        "Certified by coroner with no inquest") ~ "Certified by coroner",
      .default = {{ cert_column }}))
}

#' Add LSOA
#'
#' @param data Data frame.
#' @param nspl NSPL lookup data frame.
#'
#' @return Data frame with added lsoa column.
#' @export
#'
add_lsoa <- function(data, nspl) {

  nspl <- dplyr::mutate(nspl, pcd = gsub(" ", "", pcd))

  data %>%
    dplyr::mutate(pcdr = gsub(" ", "", pcdr)) %>%
    dplyr::left_join(nspl, by = dplyr::join_by(pcdr == pcd)) %>%
    dplyr::select(-gor, -pcdr)
}
