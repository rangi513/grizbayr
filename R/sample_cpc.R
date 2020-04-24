#' Sample Cost Per Click
#'
#' Adds 2 new nested columns to the input_df: `gamma_params` and `samples`
#'      `gamma_params` in each row should be a tibble of length 2 (\eqn{k}
#'         and \eqn{\theta} parameters)
#'     `samples` in each row should be a tibble of length `n_samples`
#'
#' See update_rules vignette for a mathematical representation.
#' \deqn{cpc_i ~ Exponential(\lambda)}
#' \deqn{\lambda ~ Gamma(\k, \theta)}
#' Average CPC is sampled from a Gamma distribution with an Exponential likelihood
#' of an individual cost.
#'
#' @param input_df Dataframe containing option_name (str), sum_clicks (dbl), sum_cost (dbl).
#' @param priors Optional list of priors {k0, theta0} for Gamma.
#'     Default \eqn{Gamma(1, 250)} will be use otherwise.
#' @param n_samples Optional integer value. Defaults to 50,000 samples.
#'
#' @importFrom purrr map map2
#' @importFrom dplyr mutate %>%
#'
#' @return input_df with 2 new nested columns `gamma_params` and `samples`
#'
sample_cpc <- function(input_df, priors, n_samples = 5e4){
  input_df %>%
    dplyr::mutate(
      gamma_params = purrr::map2(.x = sum_clicks,
                                 .y = sum_cost,
                                 ~ update_gamma(k = .x,
                                                theta = .y,
                                                priors = priors)
      ),
      samples = purrr::map(.x = gamma_params,
                           ~ rgamma(n_samples,
                                    shape = .x$k,
                                    scale = .x$theta)
      )
    )
}
