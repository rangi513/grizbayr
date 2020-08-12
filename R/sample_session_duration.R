#' Sample Session Duration
#'
#' Adds 2 new nested columns to the input_df: `gamma_params` and `samples`
#'     `gamma_params` in each row should be a tibble of length 2 (\eqn{k}
#'         and \eqn{\theta} parameters)
#'     `samples` in each row should be a tibble of length `n_samples`
#'
#' See update_rules vignette for a mathematical representation.
#' \deqn{duration_i ~ Exponential(\lambda)}
#' \deqn{\lambda ~ Gamma(\k, \theta)}
#' Session Duration is sampled from a Gamma distribution with a Exponential likelihood
#' of an individual leaving the site or ending a session at time t.
#'
#' This is not always the case, so verify your data follows the shape of
#' an exponential distribution before using this.
#'
#' @param input_df Dataframe containing option_name (str),
#'     sum_sessions (dbl), and sum_duration (dbl).
#' @param priors Optional list of priors k0 and theta0.
#'     Default \eqn{Gamma(1, 250)} will be use otherwise.
#' @param n_samples Optional integer value. Defaults to 50,000 samples.
#'
#' @importFrom purrr map map2
#' @importFrom dplyr mutate %>%
#' @importFrom rlang .data
#'
#' @return input_df with 2 new nested columns `gamma_params` and `samples`
#'
sample_session_duration <- function(input_df, priors, n_samples = 5e4){
  input_df %>%
    dplyr::mutate(
      gamma_params = purrr::map2(.x = .data$sum_sessions,
                                 .y = .data$sum_duration,
                                 ~ update_gamma(k = .x,
                                                theta = .y,
                                                priors = priors)
      ),
      samples = purrr::map(.x = .data$gamma_params,
                           ~ 1 / stats::rgamma(n_samples,
                                               shape = .x$k,
                                               scale = .x$theta)
      )
    )
}
