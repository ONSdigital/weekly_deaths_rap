#' Round 0.5 up
#'
#' Replaces R round function with janitor::round function across the
#' package. Not currently exported.
#'
#' @param x Numeric vector
#' @param digits Number of digits to round to, default 1
#'
round <- function(x, digits = 1) {
  janitor::round_half_up(x, digits)
}

#' Group by and summarise
#'
#' @description Groups by specified columns, summarises (counts). All
#' combinations of factor variable levels will be returned, even with no
#' instances.
#'
#' @param data Data frame.
#' @param grouping_cols Vector of strings of column names to group by.
#'
#' @return Data frame with counts column (ungrouped).
#' @export
#'
group_by_summarise <- function(data, grouping_cols) {
  data %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(grouping_cols)), .drop = FALSE) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::ungroup()
}
