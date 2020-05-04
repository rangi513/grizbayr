#' Estimate Win Probability vs. Baseline Given Posterior
#'
#' Calculates the win probability of the best option compared to a single other option
#' given a posterior distribution.
#'
#' @param posterior_samples Tibble returned from sample_from_posterior with 3 columns
#'   `option_name`, `samples`, and `sample_id`.
#' @param distribution String: the distribution name
#' @param wrt_option String: the option to compare against the best option.
#'
#' @return Tibble of each option_name and the win probability expressed as a percentage and a decimal `raw`
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr filter
#'
#' @examples
#' # Requires posterior_samples dataframe. See `sample_from_posterior()`
#' # for an example.
#' \donttest{
#' estimate_win_prob_vs_baseline_given_posterior(
#'     posterior_samples = posterior_samples,
#'     distribution = "conversion_rate",
#'     wrt_option = "A")
#'}
#'
estimate_win_prob_vs_baseline_given_posterior <- function(posterior_samples, distribution, wrt_option){
  validate_wrt_option(wrt_option, posterior_samples)
  best_option <- find_best_option(posterior_samples, distribution)

  posterior_samples_subset <- posterior_samples %>%
    dplyr::filter(.data$option_name %in% c(wrt_option, best_option))

  estimate_win_prob_given_posterior(posterior_samples_subset, is_winner_max(distribution))
}

