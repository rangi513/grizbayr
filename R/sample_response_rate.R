#' Sample Response Rate
#'
#' This is an alias for sample_conv_rate with a different input column.
#' Adds 2 new nested columns to the input_df: `beta_params` and `samples`
#'     `beta_params` in each row should be a tibble of length 2 (\eqn{\alpha}
#'         and \eqn{\beta} parameters)
#'     `samples` in each row should be a tibble of length `n_samples`
#'
#' See update_rules vignette for a mathematical representation.
#' \deqn{conversion_i ~ Bernoulli(\phi)}
#' \deqn{\phi ~ Beta(\alpha, \beta)}
#' Response Rate is sampled from a Beta distribution with a Binomial likelihood
#' of an individual converting.
#'
#' @param input_df Dataframe containing option_name (str), sum_conversions (dbl),
#'     and sum_sessions (dbl).
#' @param priors Optional list of priors alpha0 and beta0. Default \eqn{Beta(1,1)} will be use otherwise.
#' @param n_samples Optional integer value. Defaults to 50,000 samples.
#'
#' @importFrom dplyr rename
#'
#' @return input_df with 2 new nested columns `beta_params` and `samples`
#'
sample_response_rate <- function(input_df, priors, n_samples = 5e4){
  renamed_input_df <- dplyr::mutate(input_df, sum_clicks = sum_sessions)
  sample_conv_rate(renamed_input_df, priors, n_samples)
}

