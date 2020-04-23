#' Estimate Win Probability Given Posterior Distribution
#'
#' @param posterior_samples Tibble of data in long form with 2 columns
#' `option_name` and `samples`
#' @param winner_is_max Boolean. This should almost always be TRUE. If a larger number is better
#' then this should be TRUE. This should be FALSE for metrics such as CPA or CPC where a higher cost
#' is not necessarily better.
#'
#' @return Tibble of each option_name and the win probability expressed as a percentage and a decimal `raw`
#' @export
#' @importFrom dplyr %>% group_by filter summarise mutate arrange
#'
#' @examples
#' # Requires posterior_samples dataframe. See `sample_from_posterior()`
#' # for an example.
#' estimate_win_prob_given_posterior(posterior_samples = posterior_samples)
#' estimate_win_prob_given_posterior(posterior_samples = posterior_samples, winner_is_max = TRUE)
#'
estimate_win_prob_given_posterior <- function(posterior_samples, winner_is_max = TRUE){
  validate_posterior_samples(posterior_samples)

  wp_raw <- posterior_samples %>%
    dplyr::group_by(sample_id) %>%
    dplyr::filter(samples == if(winner_is_max) max(samples) else min(samples)) %>%
    dplyr::group_by(option_name, add = FALSE) %>%
    dplyr::summarise(win_prob_raw = dplyr::n()/nrow(.))

  wp_raw_imputed <- impute_missing_options(posterior_samples, wp_raw)

  wp_raw_imputed %>%
    dplyr::mutate(win_prob = paste0(win_prob_raw * 100, "%")) %>%
    dplyr::arrange(desc(win_prob_raw))
}
