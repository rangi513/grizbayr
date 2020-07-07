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
#' @importFrom dplyr %>% group_by filter summarise mutate arrange desc
#'
#' @examples
#' # Requires posterior_samples dataframe. See `sample_from_posterior()`
#' # for an example.
#' \donttest{
#' estimate_win_prob_given_posterior(posterior_samples = posterior_samples)
#' estimate_win_prob_given_posterior(
#'     posterior_samples = posterior_samples,
#'     winner_is_max = TRUE
#' )
#' }
#'
estimate_win_prob_given_posterior <- function(posterior_samples, winner_is_max = TRUE){
  validate_posterior_samples(posterior_samples)
  n_unique_options <- length(unique(posterior_samples$option_name))
  samples_per_option <- nrow(posterior_samples)/n_unique_options
  wp_raw <- posterior_samples %>%
    dplyr::group_by(.data$sample_id) %>%
    dplyr::filter(.data$samples == if(winner_is_max) max(.data$samples) else min(.data$samples)) %>%
    dplyr::group_by(.data$option_name) %>%
    dplyr::summarise(win_prob_raw = dplyr::n()/samples_per_option)

  wp_raw_imputed <- impute_missing_options(posterior_samples, wp_raw)

  wp_raw_imputed %>%
    dplyr::mutate(win_prob = paste0(round(.data$win_prob_raw * 100, 2), "%")) %>%
    dplyr::arrange(dplyr::desc(.data$win_prob_raw))
}
