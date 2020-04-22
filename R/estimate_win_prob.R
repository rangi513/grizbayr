#' Estimate Win Probability
#'
#' Creates a tibble of win probabilities for each option based on the data observed.
#'
#' @param input_df Dataframe containing option_name (str) and various other columns
#'     depending on the distribution type. See vignette for more details.
#' @param distribution String of the distribution name
#' @param priors Optional list of priors. Defaults will be use otherwise.
#'
#' @return tibble object with 2 columns: `option_name`
#'     and `win_probability` formatted as a percent
#' @export
#'
#' @examples
#' input_df <- tibble::tibble(
#'    option_name = c("A", "B"),
#'    sum_clicks = c(1000, 1000),
#'    sum_conversions = c(100, 120)
#' )
#' estimate_win_prob(input_df, "conversion_rate")
#'
estimate_win_prob <- function(input_df, distribution, priors = list()){
  validate_input_df(input_df, distribution)

  # Sample from posterior distribution
  posterior_samples <- sample_from_posterior(input_df, distribution, priors)

  # Calculate Win Probability
  estimate_win_prob_given_posterior(posterior_samples = posterior_samples,
                                    winner_is_max = is_winner_max(distribution))
}
