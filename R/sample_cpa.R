#' Sample Cost Per Activation (CPA)
#'
#' Adds 3 new nested columns to the input_df: `beta_params`, `gamma_params`, and `samples`
#'     `beta_params` and `gamma_params` in each row should be a tibble of length 2 (\eqn{\alpha}
#'         and \eqn{\beta} parameters and \eqn{k} and \eqn{\theta} parameters)
#'     `samples` in each row should be a tibble of length `n_samples`
#'
#'
#' See update_rules vignette for a mathematical representation.
#' This is a combination of a Beta-Bernoulli update and a Gamma-Exponential update.
#'
#' \deqn{conversion_i ~ Bernoulli(\phi)}
#' \deqn{cpc_i ~ Exponential(\lambda)}
#' \deqn{\phi ~ Beta(\alpha, \beta)}
#' \deqn{\lambda ~ Gamma(k, \theta)}
#'
#' \deqn{cpa_i ~ 1/ (Bernoulli(\phi) * Exponential(\lambda))}
#' \deqn{averageCPA ~ 1/(\phi\lambda)}
#'
#' Conversion Rate is sampled from a Beta distribution with a Binomial likelihood
#' of an individual converting.
#'
#' Average CPC is sampled from a Gamma distribution with an Exponential likelihood
#' of an individual cost.
#'
#'
#' @param input_df Dataframe containing option_name (str), sum_conversions (dbl),
#'     sum_cost (dbl), and sum_clicks (dbl).
#' @param priors Optional list of priors {alpha0, beta0} for Beta and {k0, theta0}
#'     for Gamma.
#'     Default \eqn{Beta(1,1)} and \eqn{Gamma(1, 250)} will be use otherwise.
#' @param n_samples Optional integer value. Defaults to 50,000 samples.
#'
#' @importFrom purrr map2
#' @importFrom dplyr mutate %>%
#' @importFrom stats rgamma rbeta
#'
#' @return input_df with 3 new nested columns `beta_params`, `gamma_params`, and `samples`
#'
sample_cpa <- function(input_df, priors, n_samples = 5e4){
  input_df %>%
    dplyr::mutate(
      beta_params = purrr::map2(.x = sum_conversions,
                                .y = sum_clicks,
                                ~ update_beta(alpha = .x,
                                              beta = .y - .x,
                                              priors = priors)
      ),
      gamma_params = purrr::map2(.x = sum_clicks,
                                 .y = sum_cost,
                                 ~ update_gamma(k = .x,
                                                theta = .y,
                                                priors = priors)
      ),
      samples = purrr::map2(.x = beta_params,
                            .y = gamma_params,
                            ~ 1 /( stats::rgamma(n_samples,
                                                 shape = .y$k,
                                                 scale = .y$theta) *
                                     stats::rbeta(n_samples,
                                                  shape1 = .x$alpha,
                                                  shape2 = .x$beta) )
      )
    )
}
