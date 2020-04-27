#' Estimate Win Probability vs. Baseline
#'
#' Calculates the win probability of the best option compared to a single other option
#' given an input_df
#'
#' @param input_df Dataframe containing option_name (str) and various other columns
#'     depending on the distribution type. See vignette for more details.
#' @param distribution String of the distribution name
#' @param priors Optional list of priors. Defaults will be use otherwise.
#' @param wrt_option string the option win prob is calculated with respect to (wrt). Required.
#'
#' @return Tibble of each option_name and the win probability expressed as a percentage and a decimal `raw`
#' @export
#'
#' @examples
#' input_df <- tibble::tibble(
#'     option_name = c("A", "B", "C"),
#'     sum_clicks = c(1000, 1000, 1000),
#'     sum_conversions = c(100, 120, 110)
#' )
#' estimate_win_prob_vs_baseline(input_df = input_df,
#'     distribution = "conversion_rate",
#'     wrt_option = "B")
#'
estimate_win_prob_vs_baseline <- function(input_df, distribution, priors = list(), wrt_option){
  validate_input_df(input_df, distribution)

  # Sample from posterior distribution
  posterior_samples <- sample_from_posterior(input_df, distribution, priors)

  # Calculate Win Prob vs Baseline
  estimate_win_prob_vs_baseline_given_posterior(posterior_samples, distribution, wrt_option)
}
