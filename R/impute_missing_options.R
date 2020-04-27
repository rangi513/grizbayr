#' Impute Missing Options
#'
#' When win probability is calculated
#'
#' @param posterior_samples Tibble of data in long form with 2 columns
#' `option_name` and `samples`
#' @param wp_raw Tibble of win probabilities with the columns:
#'     `option_name` and `win_prob_raw``
#'
#' @return wp_raw table with new rows if option names were missing.
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble

impute_missing_options <- function(posterior_samples, wp_raw) {
  all_option_names <- unique(posterior_samples$option_name)
  missing_option_names <- all_option_names[!all_option_names %in% wp_raw$option_name]
  if(length(missing_option_names > 0)){
    wp_raw %>%
      dplyr::bind_rows(
        tibble::tibble(option_name = missing_option_names, win_prob_raw = 0)
      )
  }else{
    wp_raw
  }
}
