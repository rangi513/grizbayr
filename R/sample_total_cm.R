#' Sample Total CM (Given Impression Count)
#'
#' Adds 4 new nested columns to the input_df: `beta_params_ctr`, `beta_params_conv`,
#'`gamma_params_rev`, `gamma_params_cost`and `samples`.
#'`beta_params` and `gamma_params` in each row should be a tibble of length 2 (\eqn{\alpha}
#'    and \eqn{\beta} parameters and \eqn{k} and \eqn{\theta} parameters).
#'`samples` in each row should be a tibble of length `n_samples`.
#' One assumption in this model is that sum_impressions is not stochastic. This assumes that
#' Clicks are stochastically generated from a set number of impressions. It does not require
#' that the number of impressions are equal on either side. Generally this assumption holds
#' true in marketing tests where traffic is split 50/50 and very little variance is observed
#' in the number of impressions on either side.
#'
#'
#' See update_rules vignette for a mathematical representation.
#'
#'
#' @param input_df Dataframe containing option_name (str), sum_conversions (dbl), sum_revenue (dbl),
#'     and sum_clicks (dbl).
#' @param priors Optional list of priors {alpha0, beta0} for Beta, {k0, theta0} for Gamma Inverse Revenue,
#'     and {k01, theta01} for Gamma Cost (uses alternate priors so they can be different from Revenue).
#'     Default \eqn{Beta(1,1) and \eqn{Gamma(1, 250)}} will be use otherwise.
#' @param n_samples Optional integer value. Defaults to 50,000 samples.
#'
#' @importFrom purrr pmap map2
#' @importFrom dplyr mutate select %>%
#'
#' @return input_df with 5 new nested columns `beta_params_conv`, `beta_params_ctr`,
#' `gamma_params_rev`,`gamma_params_cost`, and `samples`
#'
sample_total_cm <- function(input_df, priors, n_samples = 5e4){
  input_df %>%
    dplyr::mutate(
      beta_params_conv = purrr::map2(.x = sum_conversions,
                                     .y = sum_clicks,
                                     ~ update_beta(alpha = .x,
                                                   beta = .y - .x,
                                                   priors = priors)
      ),
      beta_params_ctr = purrr::map2(.x = sum_clicks,
                                    .y = sum_impressions,
                                    ~ update_beta(alpha = .x,
                                                  beta = .y - .x,
                                                  priors = priors)
      ),
      gamma_params_rev = purrr::map2(.x = sum_conversions,
                                     .y = sum_revenue,
                                     ~ update_gamma(k = .x,
                                                    theta = .y,
                                                    priors = priors)
      ),
      gamma_params_cost = purrr::map2(.x = sum_clicks,
                                      .y = sum_cost,
                                      ~ update_gamma(k = .x,
                                                     theta = .y,
                                                     priors = priors,
                                                     alternate_priors = TRUE)
      ),
      rev_per_click_samples = purrr::map2(.x = beta_params_conv,
                                          .y = gamma_params_rev,
                                          ~ rbeta(n_samples,
                                                  shape1 = .x$alpha,
                                                  shape2 = .x$beta) /
                                            rgamma(n_samples,
                                                   shape = .y$k,
                                                   scale = .y$theta)
      ),
      cost_per_click_samples = purrr::map(.x = gamma_params_cost,
                                          ~ rgamma(n_samples,
                                                   shape = .x$k,
                                                   scale = .x$theta)
      ),
      expected_clicks_samples = purrr::map(.x = beta_params_ctr,
                                           # Expected CTR samples Times Fixed Impressions
                                            ~ rbeta(n_samples,
                                                    shape1 = .x$alpha,
                                                    shape2 = .x$beta) *
                                             rep(sum_impressions, times = n_samples)
      ),
      samples = purrr::pmap(list(rev_per_click = rev_per_click_samples,
                                 cost_per_click = cost_per_click_samples,
                                 expected_clicks = expected_clicks_samples),
                            ~ calculate_total_cm(...)
      )
    ) %>%
    dplyr::select(-rev_per_click_samples, -cost_per_click_samples, -expected_clicks_samples)
}


