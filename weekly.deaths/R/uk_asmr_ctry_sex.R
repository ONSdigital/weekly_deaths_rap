#' Calculate Age-Standardised Mortality Rates and Confidence Limits
#'
#' @description Uses the `PHEindicatormethods` package created by Department of
#' Health and Social Care to calculate the Age-Standardised Mortality Rates and
#' confidence intervals.
#'
#' @param ew_df Processed England and Wales dataframe.
#' @param scot_df Processed Scotland dataframe.
#' @param ni_df Processed Northern Ireland dataframe.
#' @param config list containing year (numeric in YYYY format), week (the
#' analysis week as numeric e.g. 22) and scope (string of "UK" or "EW").
#' @param esp European Standardised Population (ESP) lookup data frame.
#' @param weekly_pop Weekly population estimates dataframe.
#'
#' @return Dataframe with PHE ASMR and CI calculated.
#' @export
calc_phe_uk_asmr <- function(ew_df, scot_df, ni_df, config, esp, weekly_pop) {

  uk_deaths <- process_uk_deaths_asmr(
    ew_df,
    scot_df,
    ni_df,
    config$year,
    config$week,
    config$scope,
    esp,
    weekly_pop
    )

  uk_deaths %>%
    calculate_asmr(grouping_vars = c("year", "week", "country", "sex")) %>%
    adjust_asmr_ci_yearly() %>%
    format_asmr()
}

#' Calculate the age-standardised or age-specific mortality rate using the DHSC
#' methodology
#'
#' @description The input is a dataframe that must contain the columns:
#' `deaths`, `population`, `esp`, `agegroup_2` as well as the grouping columns:
#' `year`, `week`, `country`, `sex`.
#' The "All ages" data is filtered out (not used to calculate the ASMR, only
#' the age groups).
#' When the total deaths is < 10 ASMRs are not reliable and will therefore be
#' suppressed in the output.
#' Further info on the method can be found here:
#' [User guide to mortality statistics](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/methodologies/userguidetomortalitystatisticsjuly2017#death-rates-ratios-and-standardisation).
#' [DHSC directly standardised rates](https://fingertips.phe.org.uk/static-reports/public-health-technical-guidance/Standardisation/DSRs.html)
#' [PHE directly standardised rates (depricated)](https://github.com/ukhsa-collaboration/PHEindicatormethods/blob/master/R/phe_DSR.R)
#'
#' 95% Confidence intervals are calculated with the Byar's method with Dobson
#' method adjustment.
#' The rate is given per 100,000.
#'
#' @param deaths_age_pop_esp Data frame with columns for `deaths`, `age_group`,
#' `population`, `esp` and additional columns to be the `grouping_vars`. This df
#' needs to have all combinations of grouping variables i.e. 0 counts where
#' there are no deaths in a grouping.
#' @param grouping_vars Vector of strings
#'
#' @return Data frame
#' @export
calculate_asmr <- function(deaths_age_pop_esp, grouping_vars) {

  if (sum(is.na(deaths_age_pop_esp)) > 0) {
    stop("There must be no missing/NA in the dataframe, check data.")
  }

  data_for_func <- deaths_age_pop_esp %>%
    dplyr::filter(age_group != "All ages") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars)))

  data_for_func %>%
    PHEindicatormethods::calculate_dsr(
      x = deaths,
      n = population,
      stdpop = esp,
      type = "standard",
      confidence = 0.95,
      multiplier = 100000,
      independent_events = TRUE,
      eventfreq = NULL,
      ageband = NULL
    ) %>%
    dplyr::rename(
      age_standardised_rate = value,
      ci_lower = lowercl,
      ci_upper = uppercl,
      deaths = total_count) %>%
    dplyr::select(-total_pop)
}


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
#' record-level. Drops Scotland counts for deaths involving Covid to leave "all
#' deaths" as Covid are duplicated counts.
#'
#' @param ew_df Processed England and Wales deaths dataframe.
#' @param scot_df Processed Scotland deaths dataframe.
#' @param ni_df Processed Northern Ireland dataframe.
#' @param current_year Analysis year as numeric in format YYYY.
#' @param current_week Week for analysis as numeric or string e.g. 1.
#' @param scope String of "UK" or "EW".
#' @param esp European Standardised Population (ESP) lookup data frame.
#' @param weekly_pop Weekly population interpolation estimates dataframe.
#' @return Dataframe of UK counts of deaths by country, age groups and sex.
#' @export
process_uk_deaths_asmr <- function(ew_df,
                                   scot_df,
                                   ni_df,
                                   current_year,
                                   current_week,
                                   scope,
                                   esp,
                                   weekly_pop) {

  weekly_pop <- population_processing_totals(weekly_pop, current_year)

  uk_deaths <- create_uk_df(ew_df, scot_df, ni_df, current_year)

  uk_deaths <- uk_deaths %>%
    dplyr::filter(year == current_year) %>%
    dplyr::select(year, week, country, sex, agegroup_2, deaths) %>%
    count_uk_dths_ctry_age_sex("agegroup_2") %>%
    add_zero_counts_uk("agegroup_2") %>%
    dplyr::left_join(
      weekly_pop,
      by = dplyr::join_by(year, week, country, sex, agegroup_2)) %>%
    dplyr::rename(age_group = agegroup_2)

  if (scope == "EW") {
    uk_deaths <- dplyr::filter(
      uk_deaths,
      !(country %in% c("Scotland", "Northern Ireland", "UK")
        & week == as.numeric(current_week)
      )
    )
  }

  # Joining on the ESP look-up:
  uk_deaths %>%
    dplyr::left_join(process_agegrp(esp, agegroup),
                     by = c("age_group" = "agegroup")) %>%
    dplyr::mutate(esp = dplyr::if_else(age_group == "All ages", 100000, esp))

}

#' Calculate population totals by country, age groups and sex
#'
#' @description Takes the processed dataframe containing estimated population in
#' the UK to totals  by week, country
#' (including UK and England/Wales/Nonresidents),
#' age group and sex. Totals are calculated for each variable (except
#' sum of weeks). The age groups are 5yrs with max "90+".
#'
#' @param data UK weekly population estimates dataframe.
#' @param current_week Week for analysis as numeric or string e.g. 1.
#' @return Dataframe with population by `week`, `country`, `agegroup_2` and
#' `sex.`
#' @export
#'
population_processing_totals <- function(weekly_population,
                                         current_year) {

  weekly_population <- weekly_population %>%
    process_agegrp(esp_age_group) %>%
    dplyr::filter(dor_year == current_year) %>%
    dplyr::select(
      year = dor_year,
      week = dor_week,
      country = area,
      agegroup_2 = esp_age_group,
      sex,
      population
    )

  # UK population by week, sex, agegroup
  uk <- weekly_population %>%
    dplyr::filter(!(country %in% c("England", "Wales"))) %>%
    total_by_group(c("sex", "agegroup_2"), "country", "UK",
                   sum_colname = "population")

  # add all country breakdown
  weekly_population <- weekly_population %>%
    dplyr::bind_rows(uk)

  # all ages totals
  age_total <- weekly_population %>%
    total_by_group(c("sex", "country"), "agegroup_2", "All ages",
                   sum_colname = "population")

  # all people totals
  sex_total <- weekly_population %>%
    total_by_group(c("country", "agegroup_2"), "sex", "All people",
                   sum_colname = "population")

  # all ages and all people
  sex_age_total <- weekly_population %>%
    total_by_group(c("country", "agegroup_2"), "sex", "All people",
                   sum_colname = "population") %>%
    total_by_group(c("country", "sex"), "agegroup_2", "All ages",
                   sum_colname = "population")

  dplyr::bind_rows(weekly_population, age_total, sex_total, sex_age_total) %>%
    dplyr::ungroup()
}


#' Convert weekly age-standardised mortality rate (ASMR) and confidence limits
#' to yearly rate
#'
#' @param data Data frame with columns for `age_standardised_rate`, `ci_upper`,
#' `ci_lower`.
#'
#' @return Data frame with the columns adjusted from weekly rates to annual
#' rates.
#' @export
#'
adjust_asmr_ci_yearly <- function(data) {
  data %>%
    dplyr::mutate(
      age_standardised_rate = mapply(
        convert_to_yearly_asmr,
        age_standardised_rate,
        year
        ),
      ci_lower = mapply(
        convert_to_yearly_asmr,
        ci_lower,
        year
        ),
      ci_upper = mapply(
        convert_to_yearly_asmr,
        ci_upper,
        year
        )
    )
}


#' Convert weekly age-standardised mortality rate (ASMR) to yearly rate
#'
#' @description This function takes the age-standardised mortality rate (ASMR)
#' calculated on a weekly basis and converts it to a yearly ASMR. This
#' annualisation of rates allows easier comparison of rate values between
#' different periods of time. More information can be found in the
#' [User guide to mortality statistics](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/methodologies/userguidetomortalitystatisticsjuly2017#death-rates-ratios-and-standardisation).
#'
#' @param weekly_asmr Numeric. The weekly age-standardised mortality rate.
#' @param year Numeric. The year for which the conversion is being done. This is
#' used to determine if the year is a leap year.
#'
#' @return Numeric. The yearly age-standardised mortality rate.
#' @examples
#' convert_to_yearly_asmr(weekly_asmr = 5, year = 2025)
#' convert_to_yearly_asmr(weekly_asmr = 5, year = 2024) # Leap year
convert_to_yearly_asmr <- function(weekly_asmr, year) {

  days_in_year <- ifelse(lubridate::leap_year(year), 366, 365)
  round((weekly_asmr / 7) * days_in_year, 1)
}



#' Format the age-standardised mortality rate (ASMR) for publication
#'
#' @description Makes the country and sex variables factors to sort them into
#' the correct order. Then renames all required columns and drops other columns.
#' Due to unreliability, if the deaths are < 10, the ASMR is suppressed and the
#' ASMR column will be a character column.
#'
#' @param data ASMR dataframe (output of `phe_age_rates`).
#'
#' @return Dataframe ready for publication or error if there would be any NA in
#' the output.
#' @export
format_asmr <- function(data) {
  lvls <- create_factor_levels()

  asmr <- data %>%
    dplyr::mutate(
      country = factor(country, lvls$country),
      sex = factor(sex, lvls$sex),
      age_standardised_rate =
        dplyr::if_else(
          deaths < 10,
          "[x]",
          as.character(
            formatC(
              age_standardised_rate,
              format = "f",
              digits = 1
              )
            )
          ),
      ci_lower =
        dplyr::if_else(
          deaths < 10,
          "[x]",
          as.character(
            formatC(
              ci_lower,
              format = "f",
              digits = 1
              )
            )
          ),
      ci_upper =
        dplyr::if_else(
          deaths < 10,
          "[x]",
          as.character(
            formatC(
              ci_upper,
              format = "f",
              digits = 1
              )
            )
          )
      ) %>%
    dplyr::arrange(-week, sex, country) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      `Week number` = week,
      Country = country,
      Sex = sex,
      `ASMR per 100,000` = age_standardised_rate,
      `Lower confidence limit` = ci_lower,
      `Upper confidence limit` = ci_upper)

  if (sum(is.na(asmr)) > 0) {
    stop("There will be unexpected NA in the ASMR table. Check the data.")
  }

  asmr
}
