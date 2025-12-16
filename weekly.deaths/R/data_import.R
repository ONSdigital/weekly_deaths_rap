#' Import data for England and Wales from the provisional files
#' @description Import of provisional death data to be joined with finalised
#' death data.
#' @details in current form this function is built to use with registration
#' data. The start date is 1st Jan of the first provisional year, as this
#' will be joined with finalised data up to 31st Dec of the previous year.
#' The config is updated in the global scope to include ew_file_final,
#' which can be used for reporting.
#' 
#' @param cfg list containing following fields:
#' \itemize{
#' \item{"year"}{analysis year as numeric e.g. 2024}
#' \item{"provisional_years"}{list of 4-digit years of provisional data}
#' \item{"var_ew"}{character vector with variable names for provisional data}
#' \item{"var_cause"}{character vector with variable names for columns with
#'  cause codes}
#' \item{"server"}{SQL server name as a string}
#' \item{"db_weekly_dths"}{database for provisional deaths data}
#' \item{"tbl_ew"}{table name for provisional deaths data}
#' }
#'
#' @return data frame with deaths data
#' @export
#'
import_ew <- function(cfg) {
  if (length(cfg$provisional_years) == 0) {
    stop(paste0("There are no years in cfg$provisional to import. ",
                "Check the config."))
  }
  year_start <- as.numeric(paste0(min(cfg$provisional_years), "0101"))
  year_end <- calc_year_end_date(max(cfg$provisional_years))

  variables <- paste(c(cfg$var_ew, cfg$var_cause), collapse = ", ")

  filter_year <- paste0(
    "(dor BETWEEN ", year_start, " AND ", year_end, ")"
  )
  cfg$ew_file_used <- paste0(cfg$db_weekly_dths, cfg$tbl_ew)

  if (cfg$year == max(cfg$provisional_years)) {
    df <- halefunctionlib::sql_data_import(
      server = cfg$server,
      database = cfg$ew_file_used,
      variables = variables,
      filter = filter_year)
  } else {
    stop(paste0("The cfg$year should match the max provisional year."),
         "Please check the config.")
  }

  cfg <<- cfg
  df
}

#' Import finalised data for England and Wales
#' @description Imports finalised death data to be joined with provisional
#' weekly deaths data
#' @details ew_file_used_final_report is saved to cfg for reporting. The end
#' date is 31st of Dec for the last finalised year as this will be joined with
#' provisional data from 1st Jan of the following year.
#' The config is updated in the global scope to include ew_file_used_final,
#' which can be used for reporting.
#' @param cfg list containing following fields:
#' \itemize{
#' \item{"finalised_years"}{numeric vector of years for finalised data (YYYY)}
#' \item{"var_cause"}{character vector with variable names for columns with
#'  cause codes}
#' \item{"server"}{SQL server name as a string}
#' \item{"db_dths"}{database for annual (finalised) deaths data}
#' \item{"tbl_ew_2018on"}{table name for annual deaths data from 2018}
#' \item{"tbl_ew_2001_2017"}{table name for annual data from 2001 to 2017}
#' \item{"var_ew_final"}{character vector of variables from finalised death data
#' tables}
#' }
#'
#' @return data frame with deaths data
#' @export
#'
import_ew_final <- function(cfg) {
  if (length(cfg$finalised_years) == 0) {
    stop(paste0("There are no years in cfg$finalised_years to import. ",
                "Check the config"))
  }
  year_start <- calc_start_date_for_year(min(cfg$finalised_years))
  year_end  <- as.numeric(paste0(max(cfg$finalised_years), "1231"))

  filter_year <- paste0(
    "(dor BETWEEN ", year_start, " AND ", year_end, ")"
  )
  var_ew_final_all <- paste(c(cfg$var_ew_final, cfg$var_cause), collapse = ", ")
  cfg$ew_file_used_final <- list(paste0(cfg$db_dths, cfg$tbl_ew_2001_2017),
                                 paste0(cfg$db_dths, cfg$tbl_ew_2018on))

  if (year_start >= 20010101) {

    df_01_17 <- halefunctionlib::sql_data_import(
      server = cfg$server,
      database = cfg$ew_file_used_final[[1]],
      variables = var_ew_final_all,
      filter = filter_year)
    df_18_on <- halefunctionlib::sql_data_import(
      server = cfg$server,
      database = cfg$ew_file_used_final[[2]],
      variables = var_ew_final_all,
      filter = filter_year)

    if (nrow(df_01_17) > 0  && nrow(df_18_on) > 0) {
      df <- dplyr::bind_rows(df_01_17, df_18_on)
      cfg$ew_file_used_final_reporting <- paste(
        cfg$ew_file_used_final, collapse = ", ")
    } else if (nrow(df_01_17) == 0 && nrow(df_18_on) > 0) {
      df <- df_18_on
      cfg$ew_file_used_final_reporting <- cfg$ew_file_used_final[[2]]
    } else if (nrow(df_01_17) > 0 && nrow(df_18_on) == 0) {
      df <- df_01_17
      cfg$ew_file_used_final_reporting <- cfg$ew_file_used_final[[1]]
    } else {
      warning("No finalised data has been imported.")
      cfg$ew_file_used_final_reporting <- "No finalised data imported."
    }
  } else {
    stop("This pipeline only supports data for years from 2001 onwards")
  }

  cfg <<- cfg
  df
}

#' Imports nspl data for England and Wales
#'
#' @param cfg list containing following fields:
#' \itemize{
#' \item{"server"}{SQL server name as a string}
#' \item{"db_ref}{SQL reference data base name as a string}
#' \item{"tbl_nspl"}{nspl table name as a string}
#' }
#'
#' @return nspl data frame with postcodes (pcd), region codes (gor) and  lower
#' layer output areas (lsoa11) for England and Wales.
#' @export
#'
import_nspl <- function(cfg) {
  halefunctionlib::sql_data_import(
    server = cfg$server,
    database = paste0(cfg$db_ref, cfg$tbl_nspl),
    variables = "pcd, gor, lsoa11",
    filter = "ctry = 'E92000001' OR ctry = 'W92000004'"
  )
}


#' Imports excess deaths data from SQL
#' @details Reads data from whole excess deaths data table from sql
#' @param config list containing fields for deaths server, excess deaths
#' database and excess deaths table.
#' @return dataframe containing excess death data including area, age, sex
#' @export
#'
import_excess_death_data_sql <- function(cfg_list) {
  halefunctionlib::sql_data_import(
    server = cfg_list$server,
    database = paste0(cfg_list$db_excess, cfg_list$tbl_excess),
    variables = cfg_list$var_excess
  )
}


#' Import NI death data
#'
#' @description This function imports death data for NI. If cfg$scope is
#' "UK" the latest week's data is imported.  If cfg$scope is "EW" the previous
#' week's data is imported. A print statement gives the week for this and
#' the NI table is updated in the config in global scope.
#' Otherwise a message is printed to the console and no data is imported.
#'
#' @details If data is imported the database, table name and variables defined
#' in the config are used.
#'
#' @param cfg_list list containing following fields:
#' \itemize{
#' \item{"scope"}{analysis scope as string e.g. "EW"}
#' \item{"year"}{analysis year as numeric e.g. 2024}
#' \item{"week"}{analysis week as numeric e.g. 15}
#' \item{"server"}{SQL server name as a string}
#' \item{"db_scot_ni"}{database name as string}
#' \item{"scot_ni_tbl"}{current scot and ni data table name as string}
#' \item{tbl_ni}{NI data table name as string}
#' \item{var_ni}{character vector with variable names for provisional data}
#' }
#'
#' @return Deaths dataframe with columns defined in the config.
#'
#' @export
#'
import_ni <- function(cfg_list) {

  if (cfg_list$scope == "EW") {
    cfg_list$scot_ni_tbl <- paste0("_2020_Week1_to_",
                                   cfg_list$year,
                                   "_Week",
                                   cfg_list$week - 1)
    cfg_list$tbl_ni <-  paste0("NISRA", cfg_list$scot_ni_tbl)
    print(paste0("Northern Ireland data was imported for week ",
                 cfg_list$week - 1))
  }
  cfg <<- cfg_list
  if (cfg_list$scope %in% c("EW", "UK")) {
    halefunctionlib::sql_data_import(
      cfg_list$server,
      paste0(cfg_list$db_scot_ni, cfg_list$tbl_ni),
      cfg_list$var_ni)
  } else {
    print("NI data not imported. The value of cfg$scope must be 'UK' or 'EW'")
  }

}


#' Import Scotland death data
#'
#' @description This function imports death data for Scotland. If cfg$scope is
#' "UK" the latest week's data is imported.  If cfg$scope is "EW" the previous
#' week's data is imported. A print statement gives the week for this and
#' the Scotland table is updated in the config in global scope
#' Otherwise a message is printed to the console and no data is imported.
#'
#' @details If data is imported the database, table name and variables defined
#' in the config are used.
#'
#' @param cfg_list list containing following fields:
#' \itemize{
#' \item{"scope"}{analysis scope as string e.g. "EW"}
#' \item{"year"}{analysis year as numeric e.g. 2024}
#' \item{"week"}{analysis week as numeric e.g. 15}
#' \item{"server"}{SQL server name as a string}
#' \item{"db_scot_ni"}{database name as string}
#' \item{"scot_ni_tbl"}{current scot and ni data table name as string}
#' \item{tbl_scot}{Scot data table name as string}
#' \item{var_scot}{character vector with variable names for provisional data}
#' }
#'
#' @return Deaths data frame with columns defined in the config.
#'
#' @export
#'
import_scot <- function(cfg_list) {

  if (cfg_list$scope == "EW") {
    cfg_list$scot_ni_tbl <- paste0("_2020_Week1_to_",
                                   cfg_list$year,
                                   "_Week",
                                   cfg_list$week - 1)
    cfg_list$tbl_scot <-  paste0("NRS", cfg_list$scot_ni_tbl)
    print(paste0("Scotand data was imported for week ", cfg_list$week - 1))
  }
  cfg <<- cfg_list
  if (cfg_list$scope %in% c("EW", "UK")) {
    halefunctionlib::sql_data_import(
      cfg_list$server,
      paste0(cfg_list$db_scot_ni, cfg_list$tbl_scot),
      cfg_list$var_scot)
  } else {
    print(paste0("Scotland data not imported. ",
                 "The value of cfg$scope must be 'UK' or 'EW'"))
  }
}

#' Import Index of Multiple Deprivation (IMD) data
#'
#' @param cfg_list
#' \itemize{
#' \item{"server"}{SQL server name}
#' \item{"db_ref}{SQL reference database name}
#' \item{"tbl_imd_england}{table name for IMD lookup for England}
#' \item{"tbl_imd_wales}{table name for IMD lookup for Wales}
#' \item{"var_imd_england"}{string of variables to import for England}
#' \item{"var_imd_wales"}{string of variable to import for Wales}}
#' @param country String value of country; either "england" or "wales".
#'
#' @return Data frame for IMD lookup table or error if `country` is not
#' "england" or "wales".
#' @export
#'
import_imd <- function(cfg_list, country) {

  if (!(country %in% c("england", "wales"))) {
    stop('country argument must be "england" or "wales"')
  }

  halefunctionlib::sql_data_import(
    server = cfg_list$server,
    database = paste0(
      cfg_list$db_ref, cfg_list[[paste0("tbl_imd_", country)]]),
    variables = cfg_list[[paste0("var_imd_", country)]])

}

#' Imports European Standard Population (ESP) lookup
#'
#' @param cfg_list List containing following fields:
#' \itemize{
#' \item{"server"}{SQL server name as a string}
#' \item{"db_pops"}{SQL population database name as a string}
#' \item{"tbl_esp"}{ESP table name as a string}
#' \item{"vars_esp"}{ESP variables as a string}
#' }
#'
#' @return ESP data frame.
#' @export
#'
import_esp <- function(cfg_list) {
  halefunctionlib::sql_data_import(
    server = cfg_list$server,
    database = paste0(cfg_list$db_pops, cfg_list$tbl_esp),
    variables = cfg_list$vars_esp
  )
}

#' Imports Poisson distribution lookup
#'
#' @description The Poisson distribution is used to calculate confidence
#' intervals e.g. for age-standardised mortality rate (ASMR).
#'
#' @param cfg_list list containing following fields:
#' \itemize{
#' \item{"server"}{SQL server name as a string}
#' \item{"db_pops"}{SQL population database name as a string}
#' \item{"tbl_poisson"}{Poisson distribution table name as a string}
#' \item{"vars_poisson"}{Poisson distribution variables as a string}
#' }
#'
#' @return Poisson distribution as data frame.
#' @export
#'
import_poisson <- function(cfg_list) {
  halefunctionlib::sql_data_import(
    server = cfg_list$server,
    database = paste0(cfg_list$db_pops, cfg_list$tbl_poisson),
    variables = cfg_list$vars_poisson
  )
}

#' Imports interpolated weekly population
#'
#' @param cfg_list list containing following fields:
#' \itemize{
#' \item{"server"}{SQL server name as a string}
#' \item{"db_excess"}{SQL reference database name as a string}
#' \item{"tbl_weekly_deaths_pops"}{Interpolated weekly population table name as
#' a string}
#' \item{"var_weekly_deaths_pops"}{Interpolated weekly population variables as a
#' string}
#' }
#'
#' @return Interpolated weekly population as data frame.
#' @export
#'
import_weekly_interpol_pops <- function(cfg_list) {
  halefunctionlib::sql_data_import(
    server = cfg_list$server,
    database = paste0(cfg_list$db_excess, cfg_list$tbl_weekly_deaths_pops),
    variables = cfg_list$var_weekly_deaths_pops
  )
}
