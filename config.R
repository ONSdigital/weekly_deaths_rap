# Variables to update ####

cfg <- list()

## Each week ####

cfg$week <- 2 # numeric
cfg$publication_date <- "22 January 2025"
cfg$scope <- "UK" # should be "EW" or "UK"

## Annually ####
cfg$year <- 2025 # in format YYYY as numeric
cfg$chart_years <- c(2023, 2024, 2025) # This should normally contain 3 years
cfg$provisional_years <- c(2025)
cfg$finalised_years <- c(2023, 2024) # This should have at least one year


# ESP
cfg$db_pops <- ""
cfg$tbl_esp <- ""
cfg$vars_esp <- c("agegroup", "esp")

# Poisson
cfg$tbl_poisson <- ""
cfg$vars_poisson <- c("deaths", "lower", "upper")

# Weekly interpolated populations
cfg$tbl_weekly_deaths_pops <- ""
cfg$var_weekly_deaths_pops <- paste0("dor_year, dor_week, esp_age_group, ",
                                     "sex, population, area")

## For SQL ####
cfg$server <- ""

cfg$db_dths <- ""
cfg$db_weekly_dths <- ""
cfg$db_scot_ni <- ""
cfg$db_excess <- ""
cfg$db_ref <- ""

cfg$tbl_nspl <- ""

cfg$tbl_ew <- ""
cfg$scot_ni_tbl <- ""
cfg$tbl_ni <-  ""
cfg$tbl_scot <- ""
cfg$tbl_ew_2018on <- ""
cfg$tbl_ew_2001_2017 <- ""
cfg$tbl_excess <- ""

cfg$var_ew <- c("dor", "reg_stat_dod", "ageinyrs", "agegroup_wide",
                "agegroup_2", "ctrynm", "dor_week", "pod", "region", "sex",
                "certtype", "pcdr")
cfg$var_ew_final <- c("dor", "dod", "sex", "ageinyrs", "agegroup_2", "ctryir",
                      "pcdr", "cestrss", "esttyped", "nhsind", "certtype")
cfg$var_cause <- c("fic10und", paste0("fic10men", 1:15))
cfg$var_ni <- "dor, dod, age, age_wide, pod, sex, agegroup_2, deaths"
cfg$var_scot <- paste0("dor, dod, placename, ageband, sex, ageband_wide, ",
                       "agegroup_2, deaths, covid")
cfg$var_ew_2018on <- "dor, ctryr, ageinyrs, sex, cestrss, nhsind, esttyped"
cfg$var_excess <- c("area, dor_year, dor_week, esp_age_group, sex,
                    expected_deaths")

cfg$tbl_imd_england <- ""
cfg$var_imd_england <- "lsoa11cd, imd_decile"
cfg$tbl_imd_wales <- ""
cfg$var_imd_wales <- "lsoa11cd, imd_quintile"

cfg$tbl_weekly_pops <- ""


## For spreadsheet ####

cfg$previous_year <- as.character(cfg$year - 1)

cfg$cover_sheet_text <- "spreadsheet_text/cover_sheet.txt"

cfg$cover_sheet_links <- c(
  "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/bulletins/deathsregisteredweeklyinenglandandwalesprovisional/latest",
  "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/articles/causeofdeathcodinginmortalitystatisticssoftwarechanges/january2022",
  "https://www.ons.gov.uk/methodology/methodologytopicsandstatisticalconcepts/disclosurecontrol/policyonprotectingconfidentialityintablesofbirthanddeathstatistics",
  "https://www.ons.gov.uk/search?q=deaths&sortBy=relevance&filter=user_requested_data&q=deaths&size=10",
  "mailto:health.data@ons.gov.uk",
  "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/",
  "mailto:psi@nationalarchives.gov.uk")

cfg$cover_sheet_links_rows <- c(17, 24, 26, 29, 32, 38, 40)

cfg$contents_text <- "spreadsheet_text/contents_table_text.txt"

cfg$notes_text <- "spreadsheet_text/notes_text.txt"

cfg$notes_sheet_links <- c(
  "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/methodologies/userguidetomortalitystatisticsjuly2017",
  "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/bulletins/deathsregisteredweeklyinenglandandwalesprovisional/latest#data-sources-and-quality",
  "https://www.nrscotland.gov.uk/publications/deaths-registered-weekly-in-scotland/",
  "https://www.nisra.gov.uk/publications/weekly-death-registrations-northern-ireland-2025",
  "https://www.nisra.gov.uk/statistics/registrar-general-quarterly-report/registrar-general-quarterly-tables",
  "https://www.gov.uk/government/collections/death-certification-reform-and-the-introduction-of-medical-examiners",
  "https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019",
  "https://www.gov.wales/welsh-index-multiple-deprivation-full-index-update-ranks-2019",
  "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/methodologies/userguidetomortalitystatisticsjuly2017",
  "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsregisteredinenglandandwalesseriesdrreferencetables"
)

cfg$notes_sheet_links_rows <- c(4, 8, 9, 10, 13, 17, 19, 20, 23, 24)

cfg$notes_sheet_finalised_link <-
  "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsregisteredsummarystatisticsenglandandwales"

cfg$table_headings <-list(
  table_1  = "Weekly provisional death registrations by sex, age group, Index of Multiple Deprivation (IMD) and place of occurrence, England and Wales",
  table_2  = paste0("Weekly provisional death occurrences by sex, age group, Index of Multiple Deprivation (IMD) and place of occurrence, registered ", cfg$previous_year, " and ", cfg$year, ", England and Wales"),
  table_3  = "Weekly provisional death registrations for selected causes of death by country and region, England and Wales",
  table_4  = "Weekly provisional figures on registration delays by death certification type and place of occurrence, for deaths registered that week, England and Wales",
  table_5  = "Weekly provisional death registrations and excess deaths by sex and age group, United Kingdom - Official Statistics in Development",
  table_6  = "Weekly provisional death registrations by country, sex and age group, United Kingdom",
  table_7  = "Weekly provisional annualised age-standardised mortality rates (ASMR) by country and sex, United Kingdom"
  )

cfg$table_note_numbers <-list(
  table_1  = "[note 1][note 2][note 3][note 17][note 19][note 20][note 21]",
  table_2  = "[note 1][note 2][note 3][note 17][note 19][note 20][note 21]",
  table_3  = "[note 1][note 4][note 5][note 6][note 7][note 17]",
  table_4  = "[note 1][note 17][note 18][note 22]",
  table_5  = "[note 1][note 2][note 8][note 13][note 14][note 15][note 16][note 17]",
  table_6  = "[note 1][note 2][note 9][note 10][note 11][note 12][note 13][note 17]",
  table_7  = "[note 1][note 9][note 10][note 11][note 12][note 13][note 17][note 23]"
  )

cfg$add_text <- c(
  "Some cells refer to notes which can be found on the notes worksheet.",
  "Source: Office for National Statistics"
  )
