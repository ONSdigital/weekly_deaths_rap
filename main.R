# Load packages, import config -------------------------------------------------

# library(weekly.deaths)
devtools::load_all("weekly.deaths")
source("config.R")

# Data import ------------------------------------------------------------------

ew_deaths <- import_ew(cfg)

ew_final <- import_ew_final(cfg)

ni_deaths <- import_ni(cfg)

scot_deaths <- import_scot(cfg)

exp_deaths_sql <- import_excess_death_data_sql(cfg)

nspl <- import_nspl(cfg)

imd <- list(
  england = import_imd(cfg, "england"),
  wales = import_imd(cfg, "wales")
)

esp <- import_esp(cfg)

weekly_deaths_pops <- import_weekly_interpol_pops(cfg)

# Data validation --------------------------------------------------------------

validation_report <- validate_data(ew_deaths)
weekly_pops_valid_report <- validate_weekly_pop(weekly_deaths_pops)

# Data processing --------------------------------------------------------------

ew_deaths <- ew_deaths %>%
  process_ew_weekly_deaths(nspl, derive_max_reg_date(cfg)) %>%
  derive_years_week_numbers(cfg$year)

ew_final <- ew_final %>%
  process_ew_annual(nspl) %>%
  derive_years_week_numbers(cfg$year)

ew_deaths <- dplyr::bind_rows(ew_deaths, ew_final)

# Drop records before the year start date
ew_deaths_single_year <- filter_before_year_start(ew_deaths, cfg$year)

# max_reg_date is different for Scotland as different ISO week
scot_deaths <- scot_deaths %>%
  process_scot_weekly_deaths(cfg) %>%
  derive_years_week_numbers(cfg$year, is_scot = TRUE)

ni_deaths <- ni_deaths %>%
  process_ni_weekly_deaths(derive_max_reg_date(cfg), cfg$scope) %>%
  derive_years_week_numbers(cfg$year)

exp_deaths_uk <- process_excess_deaths(exp_deaths_sql, cfg$year, cfg$week)

# Create tables -----------------------------------------------------------

ew_registrations <- ew_deaths_single_year %>%
  count_dths_geo_age_sex_pod(cfg$year, cfg$week) %>%
  bind_imd_values(ew_deaths_single_year, cfg$week, cfg$year, imd) %>%
  format_dths_geog_age_sex_pod_imd()

ew_occurrences <- create_ew_occurrences_table(
  ew_deaths_single_year,
  ew_deaths,
  imd,
  cfg)

ew_registrations_by_cause <- ew_deaths_single_year %>%
  count_dths_cause_geo() %>%
  format_dths_cause_geo()

ew_registration_delays <- ew_deaths_single_year %>%
  create_delay_summary(c("week", "week_ending")) %>%
  format_delay_summary()

ew_reg_delays_cert_pod_area <- ew_deaths_single_year %>%
  create_delay_area_pod_cert_sum() %>%
  format_delay_summary_certtype()

uk_excess_deaths <- ew_deaths_single_year %>%
  create_uk_dths_ctry_age_sex(
    scot_deaths,
    ni_deaths,
    cfg$year,
    cfg$week,
    cfg$scope,
    "agegroup_2") %>%
  calc_excess_deaths_uk(exp_deaths_uk, cfg) %>%
  format_excess_deaths_model()


uk_registrations <-   ew_deaths_single_year %>%
  create_uk_dths_ctry_age_sex(
    scot_deaths,
    ni_deaths,
    cfg$year,
    cfg$week,
    cfg$scope) %>%
  format_uk_dths_ctry_age_sex()


uk_asmr <- ew_deaths_single_year %>%
  calc_phe_uk_asmr(scot_deaths, ni_deaths, cfg, esp, weekly_deaths_pops)


dashboard <- ew_deaths %>%
  create_dashboard_data(
    exp_deaths_sql,
    scot_deaths,
    ni_deaths,
    cfg,
    num_yr_to_keep = 2) %>%
  format_dashboard(cfg, 4)

# Setup options-----------------------------------------------------------------

# Prevents scientific notation of numerics (i.e. standard form)
options("scipen" = 999)

# Create publication ready file ------------------------------------------------
wb <- openxlsx::createWorkbook(
  creator = NULL,
  title = paste(
    "Deaths registered weekly in England and Wales, provisional: published",
    cfg$publication_date)
)

cover_sheet_text <- read.csv(
  cfg$cover_sheet_text,
  header = FALSE,
  sep = "\t",
  fileEncoding = "UTF-8-BOM"
  )

cover_sheet_links <- data.frame(
  rows = cfg$cover_sheet_links_rows,
  links = cfg$cover_sheet_links
  )
replacements <- data.frame(
  replace = c("!release_date!", "!current_year!"),
  with = c(cfg$publication_date, format(Sys.Date(), format = "%Y"))
  )
rapid.spreadsheets::create_cover_sheet(
  wb,
  text_df = cover_sheet_text,
  subheadings = c(2, 5, 7, 16, 18, 21, 27, 34),
  rows_to_bold = c(2:4),
  hyperlinks = cover_sheet_links,
  replacements = replacements
)

contents_sheet_text <- read.csv(
  cfg$contents_text,
  header = TRUE,
  sep = "\t",
  fileEncoding = "UTF-8-BOM",
  check.names = FALSE
)

rapid.spreadsheets::create_contents_notes(
  wb,
  df = contents_sheet_text,
  border_type = "vertical"
)

notes_sheet_text <- read.csv(
  cfg$notes_text,
  header = TRUE,
  sep = "\t",
  quote = "",
  fileEncoding = "UTF-8-BOM",
  check.names = FALSE
)
notes_sheet_links_df <- data.frame(
  rows = cfg$notes_sheet_links_rows,
  links = cfg$notes_sheet_links
)

# Note on finalised data only needed if prev year is finalised
if ((cfg$year - 1) %in% cfg$finalised_years){
  notes_sheet_text <- dplyr::mutate(
    notes_sheet_text,
    `Note text` = stringr::str_replace_all(
      `Note text`, "!previous_year!", as.character(cfg$previous_year)))

  cfg$table_note_numbers$table_2 <- paste0(
    cfg$table_note_numbers$table_2,
    "[note ", nrow(notes_sheet_text), "]")
} else {
  notes_sheet_text <- dplyr::slice(notes_sheet_text, -dplyr::n())
  notes_sheet_links_df <- dplyr::slice(notes_sheet_links_df, -dplyr::n())
}

rapid.spreadsheets::create_contents_notes(
  wb,
  df = notes_sheet_text,
  tab_name = "Notes",
  heading = "Notes",
  contents_links = FALSE,
  hyperlinks = notes_sheet_links_df,
  column_width = c(20, 80, 40)
)

rapid.spreadsheets::create_data_table_tab(
  wb,
  df = ew_registrations,
  tab_name = "Table_1",
  heading = cfg$table_headings$table_1,
  column_width = c(10, 16, 28, 10, 15, 20, 20, 10),
  no_decimal = 8,
  left_align = 1,
  border_type = "vertical",
  additional_text = c(
    cfg$table_note_numbers$table_1,
    cfg$add_text)
)
rapid.spreadsheets::create_data_table_tab(
  wb,
  df = ew_occurrences,
  tab_name = "Table_2",
  heading = cfg$table_headings$table_2,
  column_width = c(10, 10, 16, 28, 10, 15, 20, 20, 10),
  no_decimal = 9,
  left_align = c(1, 2),
  border_type = "vertical",
  additional_text = c(
    cfg$table_note_numbers$table_2,
    paste0("This table includes death occurrences registered by ",
           ew_occurrences$`Week ending`[1], "."),
    cfg$add_text)
)

rapid.spreadsheets::create_data_table_tab(
  wb = wb,
  df = ew_registrations_by_cause,
  tab_name = "Table_3",
  heading = cfg$table_headings$table_3,
  column_width = c(10, 16, 30, 55, 10),
  no_decimal = 5,
  left_align = 1,
  border_type = "vertical",
  additional_text = c(
    cfg$table_note_numbers$table_3,
    cfg$add_text)
)

rapid.spreadsheets::create_data_table_tab(
  wb = wb,
  df = ew_reg_delays_cert_pod_area,
  tab_name = "Table_4",
  heading = cfg$table_headings$table_4,
  column_width = c(10, 16, 30, 30, 16, 15, 25, 25, 25, 25, 25),
  no_decimal = c(6, 7, 9, 10, 11),
  one_decimal = 8,
  left_align = 1,
  border_type = "vertical",
  additional_text = c(
    cfg$table_note_numbers$table_4,
    cfg$add_text[1],
    paste0("Some cells in this table are empty for the registration delay ",
           "measures where there are no deaths for the category and therefore ",
           "measures cannot be calculated."),
    cfg$add_text[2])
)

rapid.spreadsheets::create_data_table_tab(
  wb = wb,
  df = uk_excess_deaths,
  tab_name = "Table_5",
  heading = cfg$table_headings$table_5,
  column_width = c(10, 28, 10, 10, 10, 10, 10, 11),
  no_decimal = c(5, 6, 7),
  one_decimal = 8,
  left_align = 1,
  border_type = "vertical",
  additional_text = c(
    cfg$table_note_numbers$table_5,
    cfg$add_text)
)

rapid.spreadsheets::create_data_table_tab(
  wb,
  df = uk_registrations,
  tab_name = "Table_6",
  heading = cfg$table_headings$table_6,
  column_width = c(10, 28, 10, 10, 10),
  no_decimal = 5,
  left_align = 1,
  border_type = "vertical",
  additional_text = c(
    cfg$table_note_numbers$table_6,
    cfg$add_text)
)

rapid.spreadsheets::create_data_table_tab(
  wb,
  df = uk_asmr,
  tab_name = "Table_7",
  heading = cfg$table_headings$table_7,
  column_width = c(10, 31, 10, 9, 10, 10),
  num_char_col = c(4:6),
  one_decimal = c(4:6),
  left_align = 1,
  border_type = "vertical",
  additional_text = c(
    cfg$table_note_numbers$table_7,
    cfg$add_text)
)


# Save outputs -----------------------------------------------------------------

time_stamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
week_time <- paste0(as.character(cfg$week), "_", time_stamp)

create_run_log(cfg, time_stamp)

rmarkdown::render(
  input = "quality_report.Rmd",
  output_file = paste0(week_time, "_quality_report.html"),
  output_dir = "outputs")

openxlsx::saveWorkbook(
  wb,
  paste0("outputs/weekly_deaths_week_", week_time, ".xlsx"))

write.csv(
  dashboard,
  paste0("outputs/weekly_deaths_dashboard_w_", week_time, ".csv"),
  row.names = FALSE)
