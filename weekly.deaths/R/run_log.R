#' Create a run log
#'
#' @description Produces a .txt file that is saved to the outputs folder and
#' contains information about the time the RAP is run, user running the code,
#' location of the code, week and years used, and server and tables used.
#'
#' @param cfg a list which includes,  week_no, publication_date,
#' years_for_averages_finalised, years_for_averages_provisional and details of
#' server and tables used for death data
#' @param time_stamp Time as character string, unified with other saved outputs.
#'
#' @export
#'
create_run_log <- function(cfg, timestamp) {

  file_conn <- file(paste0("./outputs/run_log_", "week_",
                           as.character(cfg$week), "_",
                           time_stamp, ".txt"),
                    open = "a")

  user <- stringr::str_glue("User: {Sys.getenv('USERNAME')}")
  date <- stringr::str_glue("Run date time: {Sys.time()}")
  file <- stringr::str_glue(
    "File run: {rstudioapi::getSourceEditorContext()$path}")
  writeLines(c(user, date, file, "\n"), file_conn)

  week_end_date <- format(
    as.Date(derive_max_reg_date(cfg), "%Y%m%d"),
    "%d %B %Y"
  )

  file_info <- stringr::str_glue(
    "Output file is week {cfg$week} {cfg$year} ",
    "for publication {cfg$publication_date}")
  max_date <- stringr::str_glue(
    "The registration week end date is {week_end_date}")
  writeLines(c(file_info, max_date, "\n"), file_conn)
  years_for_av <- stringr::str_glue(
    "Data years used in the pipeline\n",
    "* finalised: {paste(cfg$finalised_years, collapse = ', ')}\n",
    "* provisional: ",
    "{paste(cfg$provisional_years, collapse = ', ')}")
  writeLines(c(years_for_av, "\n"), file_conn)

  server <- stringr::str_glue("Server: {cfg$server}")
  ew_table <- stringr::str_glue(
    "England and Wales data: {cfg$ew_file_used}")
  ew_final_table <- stringr::str_glue(
    "England and Wales finalised data: ",
    "{cfg$ew_file_used_final_reporting}")
  scot_table <- stringr::str_glue(
    "Scotland data: {paste0(cfg$db_scot_ni, cfg$tbl_scot)}")
  ni_table <- stringr::str_glue(
    "Northern Ireland data: {paste0(cfg$db_scot_ni, cfg$tbl_ni)}")
  writeLines(c(ew_table, ew_final_table, scot_table, ni_table, "\n"), file_conn)

  close(file_conn)
}
