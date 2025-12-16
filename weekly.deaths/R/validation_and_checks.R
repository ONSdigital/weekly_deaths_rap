#' Error if required columns not present
#'
#' @description Stops the execution of the function and returns an error
#' message stating which of the required columns are missing.
#'
#' @param data Dataframe to check.
#' @param required_cols Vector of strings of the required columns.
#'
#' @return Error message stating which of the required columns are missing
#' @export
check_columns_present <- function(data, required_cols) {
  if (!all(required_cols %in% colnames(data))) {
    missing_cols <- required_cols[!required_cols %in% colnames(data)]
    stop(
      "Missing required columns: ", paste(missing_cols, collapse = ", "), "."
    )
  }
  data
}

#' Validates deaths weekly EW data
#'
#' @description Uses subfunctions depending on the column of data frame to
#' validate if the specific column content is expected format.
#' The results of the validation are compiled into a report, displaying the
#' count and indices of pass, fail, and null entries for each column.
#'
#' @details All subfunctions allow for NA, plus specific validations.
#'
#' @param data Unprocessed deaths dataframe from SQL, weekly deaths.
#'
#' @return Report list object containing each of the checks by counts and
#' indices of the passing, failing and nulls in each one. Returns warning if
#' there are any failing records.
#' @export
validate_data <- function(data) {

  # List of validation reports for each column:
  report <- list(
    dor = generate_report(data$dor, is_valid_date_format),
    dod = generate_report(data$reg_stat_dod, is_valid_date_format),
    ageinyrs = generate_report(data$ageinyrs, is_valid_age),
    agegroup_wide = generate_report(data$agegroup_wide, is_valid_agegrp_wide),
    agegroup = generate_report(data$agegroup_2, is_valid_agegrp),
    country = generate_report(data$ctrynm, is_valid_country),
    dor_week = generate_report(data$dor_week, is_valid_week_num),
    place_of_death = generate_report(data$pod, is_valid_pod),
    region = generate_report(data$region, is_valid_region),
    sex = generate_report(data$sex, is_valid_sex),
    certtype = generate_report(data$certtype, is_valid_certtype),
    fic10und = generate_report(data$fic10und, is_valid_icd_format)
  )

  for (i in 1:15) {
    column_name <- paste0("fic10men", i)
    report[[column_name]] <- generate_report(
      data[[column_name]], is_valid_icd_format
    )
  }

  check_region_country(data)

  report
}


#' Generate a validation report for a particular column
#'
#' @param col Column to apply validation to, in format data$column.
#' @param valid_func The validation function to apply to the column.
#'
#' @return List output of validation report.
#' @export
generate_report <- function(col, valid_func) {

  valid_results <- valid_func(col)

  results <- list(
    pass_count = length(which(valid_results)),
    fail_count = length(which(!valid_results)),
    null_count = length(which(is.na(col))),
    fail_indices = which(!valid_results),
    null_indices = which(is.na(col))
  )

  if (results$fail_count != 0) {
    warning(
      paste0("There are ", results$fail_count, " records that fail validation",
             " for ", substitute(valid_func), ". Check validation report."))
  }

  results
}


#' Data validation: date columns (e.g. dod and dor)
#'
#' @description Check if either string or integer in of length 8
#' YYYYMMDD format or is NULL/NA.
#'
#' @param x Vector to validate.
#' @param min What is the minimum potential date the data can be, default
#' 19000000.
#' @param max What is the maximum potential date the data can be, default is
#' the system time (run date).
#'
#' @return Logical vector.
#' @export
is_valid_date_format <- function(x,
                                 min = 19000000,
                                 max = format(Sys.Date(), "%Y%m%d")) {
  (grepl("^([0-9]{8}|NA)$", as.character(x)) & x >= min & x <= max) | is.na(x)
}


#' Data validation: any string column of specific length
#'
#' @description Check string is of a specified length (default 7) or is NULL/NA.
#'
#' @param x Vector to validate.
#'
#' @return TRUE or FALSE depending if the data is in valid format.
#' @export
is_valid_char_length <- function(x, len = 7) {
  nchar(x) == len | is.na(x)
}


#' Data validation: age column
#'
#' @description Check input is a numeric value or is NULL/NA, input is between 0
#' and 130 (for min and max valid ages).
#'
#' @param x Vector to validate.
#'
#' @return TRUE or FALSE depending if the data is in valid format.
#' @export
is_valid_age <- function(x, max = 130) {
  x %in% 0:max | is.na(x)
}


#' Data validation: sex and nhsind columns
#'
#' @description Check input is either 1, 2, or NULL/NA.
#'
#' @param x Vector to validate.
#'
#' @return TRUE or FALSE depending if the data is in valid format.
#' @export
is_valid_sex <- function(x) {
  x %in% c(1, 2) | is.na(x)
}


#' Data validation: ICD formatted columns (cause of death)
#'
#' @description Check string starts with a letter followed by at least two
#' numbers, or is NULL/NA.
#'
#' @param x Vector to validate.
#'
#' @return TRUE or FALSE depending if the data is in valid format.
#' @export
is_valid_icd_format <- function(x) {
  grepl("^[A-Za-z][0-9]{2}", x) | is.na(x)
}


#' Data validation: age group column
#'
#' @description Check input is present in the available levels for the type of
#' agegroups in the column.
#'
#' @param x Vector to validate.
#' @param wide wide TRUE if the column in wide format (large age bands), default
#' is FALSE.
#'
#' @return TRUE or FALSE depending if the data is in valid format.
#' @export
is_valid_agegrp <- function(x) {
  x %in% c("<1", "01-04", "05-09", "10-14", "15-19", "20-24", "25-29",
           "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
           "65-69", "70-74", "75-79", "80-84", "85-89", "90+") |
    is.na(x)
}

#' Data validation: age group wide column
#'
#' @description Check input is present in the available levels for the type of
#' agegroups in the column.
#'
#' @param x Vector to validate.
#'
#' @return TRUE or FALSE depending if the data is in valid format.
is_valid_agegrp_wide <- function(x) {
  x %in% c("<1", "01-14", "15-44", "45-64", "65-74", "75-84", "85+") |
    is.na(x)
}

#' Data validation: country and region
#'
#' @description Check contents of country and region and makes sure they
#' correctly correspond.
#'
#' @details Checks regions correspond to England and Wales corresponds to Wales.
#' Non-residents correspond to NA in country. Note: as this data is unprocessed,
#' the region is "East of England", rather than "East". "East of England" should
#' be used in the publication.
#'
#' @param data Unprocessed deaths data.
#'
#' @return Error message if there are any mismatches.
check_region_country <- function(data) {
  check <- data %>%
    dplyr::mutate(correct = dplyr::case_when(
      (region %in% create_factor_levels()$region &
         ctrynm == "England") |
        (region == "Wales" & ctrynm == "Wales") |
        (region == "Nonresident" & is.na(ctrynm)) |
        (is.na(region) & is.na(ctrynm)) ~ TRUE,
      TRUE ~ FALSE))

  if (sum(check$correct) < nrow(data)) {
    stop("Mismatch between country and region. Check data.")
  }
}



#' Data validation: country column (ctrynm)
#'
#' @description Check input is either "England", "Wales", or NULL/NA.
#'
#' @param x Vector to validate.
#'
#' @return TRUE or FALSE depending if the data is in valid format.
is_valid_country <- function(x) {
  x %in% c("England", "Wales") | is.na(x)
}


#' Data validation: region column
#'
#' @description Check input is one of the regions or NULL/NA.
#'
#' @param x Vector to validate.
#'
#' @return TRUE or FALSE depending if the data is in valid format.
is_valid_region <- function(x) {
  x %in% c(create_factor_levels()$region, "Nonresident", "Wales") |
    is.na(x)
}


#' Data validation: week number in the year
#'
#' @description Check input is numeric between -53 and 53, or NULL/NA.
#'
#' @param x Vector to validate.
#'
#' @return TRUE or FALSE depending if the data is in valid format.
is_valid_week_num <- function(x) {
  x %in% c(-53:53) | is.na(x)
}


#' Data validation: place of death
#'
#' @description Check input is numeric between 1 and 53, or NULL/NA.
#'
#' @param x Vector to validate.
#'
#' @return TRUE or FALSE depending if the data is in valid format.
is_valid_pod <- function(x) {
  x %in% c("Care home", "Elsewhere", "Home", "Hospice", "Hospital",
           "Other communal establishment") |
    is.na(x)
}

#' Data validation: cestrss column
#'
#' @details Not using this test for now, no unit test yet.
#'
#' @description Check string is either "H", "E", a 5-digit number, or NULL/NA.
#'
#' @param x Vector to validate.
#'
#' @return TRUE or FALSE depending if the data is in valid format.
is_valid_cestrss <- function(x) {
  x %in% c("H", "E") | grepl("^[0-9]{5}$", x) | is.na(x)
}

#' Data validation: certtype column
#'
#' @param x Check input is numeric between 1 to 14 or NA.
#'
#' @return Vector to validate.
#' @export
#'
is_valid_certtype <- function(x) {
  x %in% c(1:14) | is.na(x)
}

#' Check the week and year derivative from the data
#'
#' @description Uses `dor` and `reg_stat_dod` derived publication weeks and
#' years to check if there are any dates that are unable to be derived or
#' impossible values (registration before occurrence or negative).
#' A small proportion of deaths are registered on the same day as they occur,
#' but none should be registered before they occur.
#' It reports the number of records that could not be assigned a week or year
#' number.
#'
#' @details IDEA: Add a second argument of the quality report to save the output
#' of this function, rather than just a message.
#'
#' @param data Processed deaths dataframe.
#'
#' @return List of counts of NA or impossible week numbers or years. Message of
#' how many dates are unable to be derived.
#' @export
pub_week_year_validation <- function(data) {

  dor_before_dod <- data %>%
    dplyr::mutate(if_impossible = ifelse(reg_stat_dod > dor, TRUE, FALSE))

  # Informing the user if a week or year number couldn't be derived and how
  # many:
  valid_list <- list(
    dor_week_na_count = sum(is.na(data$dor_pub_week)),
    dor_year_na_count = sum(is.na(data$dor_pub_year)),
    dod_week_na_count = sum(is.na(data$reg_stat_dod_pub_week)),
    dod_year_na_count = sum(is.na(data$reg_stat_dod_pub_year)),
    dor_before_dod = sum(dor_before_dod$if_impossible, na.rm = TRUE)
  )

  if (valid_list$dor_week_na_count > 0 || valid_list$dod_week_na_count > 0) {
    message(paste(
      "Number of records that couldn't be assigned with a week number:",
      "Registrations:", valid_list$dor_week_na_count, "Occurrences:",
      valid_list$dod_week_na_count
    ))
  }

  if (valid_list$dor_year_na_count > 0 || valid_list$dod_year_na_count > 0) {
    message(paste(
      "Number of records that couldn't be assigned with a year:",
      "Registrations:", valid_list$dor_year_na_count, "Occurrences:",
      valid_list$dod_year_na_count
    ))
  }

  if (valid_list$dor_before_dod > 0) {
    message(paste(
      "There are", valid_list$dor_before_dod, "records that have registration",
      "before occurences. Check data."
    ))
  }
  valid_list
}

#' Validates weekly interpolated population
#'
#' @description Uses subfunctions depending on the column of data frame to
#' validate if the specific column content is expected format.
#' The results of the validation are compiled into a report, displaying the
#' count and indices of pass, fail, and null entries for each column.
#'
#'  @details Specifically fails (errors) if there are any NA in the whole
#'  dataframe.
#'
#' @param data Unprocessed weekly interpolated dataframe from SQL (output of the
#' excess deaths pipeline).
#'
#' @return Report list object containing each of the checks by counts and
#' indices of the passing, failing and nulls in each one. Returns warning if
#' there are any failing records.
#' @export
validate_weekly_pop <- function(data) {

  if (sum(is.na(data)) > 0) {
    stop("There are NA in the weekly population data. Check it.")
  }

  # List of validation reports for each column:
  list(
    dor_year = generate_report(data$dor_year, is_valid_year),
    dor_week = generate_report(data$dor_week, is_valid_dor_week),
    esp_age_group = generate_report(data$esp_age_group, is_valid_agegrp),
    sex = generate_report(data$sex, is_valid_sex_str),
    population = generate_report(data$population, is_valid_pop),
    area = generate_report(data$area, is_valid_ctry_uk)
  )
}


#' Data validation: year
#'
#' @param x Check input is positive numeric integer of length 4, and not
#' exceeding the current year, not NA.
#'
#' @return TRUE or FALSE depending if the data is in valid format.
#' @export
#'
is_valid_year <- function(x) {
  nchar(x) == 4 & x %% 1 == 0 & x > 0 &
    x <= format(Sys.Date(), "%Y") & !is.na(x)
}

#' Data validation: week number
#'
#' @param x Check input is positive numeric integer of range 1 to 53, not NA.
#'
#' @return TRUE or FALSE depending if the data is in valid format.
#' @export
#'
is_valid_dor_week <- function(x) {
  x %in% 1:53
}

#' Data validation: country (UK level)
#'
#' @param x Check input is one of the correct country strings
#' ("England, Wales and non-residents", "England", "Wales", "Northern Ireland",
#' "Scotland", "Wales"), not NA.
#'
#' @return TRUE or FALSE depending if the data is in valid format.
#' @export
#'
is_valid_ctry_uk <- function(x) {
  x %in% c("England, Wales and non-residents", "England", "Wales",
           "Northern Ireland", "Scotland")
}

#' Data validation: sex as string
#'
#' @description Check input is either "Female" or "Male", not NA.
#'
#' @param x Vector to validate.
#'
#' @return TRUE or FALSE depending if the data is in valid format.
#' @export
is_valid_sex_str <- function(x) {
  x %in% c("Female", "Male")
}

#' Data validation: population
#'
#' @description Check input is positive and numeric, not NA.
#'
#' @param x Vector to validate.
#'
#' @return TRUE or FALSE depending if the data is in valid format.
#' @export
is_valid_pop <- function(x) {
  is.numeric(x) & x > 0 & !is.na(x)
}
