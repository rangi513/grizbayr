#' Estimate Win Probability Given Posterior Distribution
#'
#' @param posterior_samples Tibble of data in long form with 2 columns
#' `option_name` and `samples`
#'
#' @return Tibble of each option_name and the win probability expressed as a percentage and a decimal `raw`
#' @export
#' @importFrom dplyr %>% group_by filter summarise mutate arrange
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
