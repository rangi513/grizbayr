#' Find Best Option
#'
#' Samples from posterior, calculates win probability, and selects the best option.
#' Note: this can be inefficient if you already have the win probability dataframe.
#' Only use this if that has not already been calculated.
#'
#' @param posterior_samples Tibble returned from sample_from_posterior with 3 columns
#'   `option_name`, `samples`, and `sample_id`.
#' @param distribution String: name of the distribution
#'
#' @return String: the best option name
#'
#' @importFrom dplyr filter
#' @importFrom magrittr extract2 %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Requires posterior distribution
#' \donttest{
#' find_best_option(posterior_samples = posterior_samples, distribution = "conversion_rate")
#' }
find_best_option <- function(posterior_samples, distribution){
  estimate_win_prob_given_posterior(posterior_samples = posterior_samples,
                                    winner_is_max = is_winner_max(distribution)) %>%
    dplyr::filter(.data$win_prob_raw == max(.data$win_prob_raw)) %>%
    magrittr::extract2("option_name")
}
