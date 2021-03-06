#' Sample Click Through Rate
#'
#' This is an alias for sample_conv_rate with 2 different input
#' columns. This function calculates posterior samples of
#' \eqn{CTR = clicks/impressions}. Adds 2 new nested columns to
#' the input_df: `beta_params` and `samples`.
#' `beta_params` in each row should be a tibble of length 2 (\eqn{\alpha}
#' and \eqn{\beta} parameters)
#' `samples` in each row should be a tibble of length `n_samples`
#'
#' See update_rules vignette for a mathematical representation.
#' \deqn{click_i ~ Bernoulli(\phi)}
#' \deqn{\phi ~ Beta(\alpha, \beta)}
#' Click Through Rate is sampled from a Beta distribution with a Binomial
#' likelihood of an individual Clicking
#'
#' @param input_df Dataframe containing option_name (str),
#'     sum_clicks (dbl), and sum_impressions (dbl).
#' @param priors Optional list of priors alpha0 and beta0.
#'     Default \eqn{Beta(1,1)} will be use otherwise.
#' @param n_samples Optional integer value. Defaults to 50,000 samples.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
#' @return input_df with 2 new nested columns `beta_params` and `samples`
#'
sample_ctr <- function(input_df, priors, n_samples = 5e4){
  renamed_input_df <- dplyr::mutate(input_df,
                                    sum_conversions = .data$sum_clicks,
                                    sum_clicks = .data$sum_impressions)
  sample_conv_rate(renamed_input_df, priors, n_samples)
}

