#' Sample Multiple Revenue Per Session
#'
#' Adds 5 new nested columns to the input_df: `beta_params_A`, `beta_parms_B`,
#'     `gamma_params_A`, `gamma_params_B`, and `samples`.
#'     `beta_params` in each row should be a tibble of length 2 (\eqn{\alpha}
#'         and \eqn{\beta} parameters)
#'     `samples` in each row should be a tibble of length `n_samples`
#'
#' @param input_df Dataframe containing option_name (str), sum_conversions (dbl),
#'     sum_sessions (dbl), sum_revenue (dbl), sum_conversion_2 (dbl), sum_sessions_2 (dbl),
#'     sum_revenue_2 (dbl).
#' @param priors Optional list of priors alpha0 and beta0. Default \eqn{Beta(1,1)} will be use otherwise.
#' @param n_samples Optional integer value. Defaults to 50,000 samples.
#'
#' @importFrom purrr map map2
#' @importFrom dplyr mutate
#'
#' @return input_df with 2 new nested columns `beta_params` and `samples`
#'
sample_multi_rev_per_session <- function(input_df, priors, n_samples = 5e4){
  input_df %>%
    dplyr::mutate(
      no_clicks = sum_sessions - sum_conversions - sum_conversions_2,
      dirichlet_params = purrr::pmap(.l = list(sum_conversions = sum_conversions,
                                               sum_conversions_2 = sum_conversions_2,
                                               no_clicks = no_clicks),
                                  ~ update_dirichlet(alpha_0 = no_clicks,
                                                     alpha_1 = sum_conversions,
                                                     alpha_2 = sum_conversions_2,
                                                     priors = priors)
      ),
      gamma_params_A = purrr::map2(.x = sum_conversions,
                                   .y = sum_revenue,
                                   ~ update_gamma(k = .x,
                                                  theta = .y,
                                                  priors = priors)
      ),
      gamma_params_B = purrr::map2(.x = sum_conversions_2,
                                   .y = sum_revenue_2,
                                   ~ update_gamma(k = .x,
                                                  theta = .y,
                                                  priors = priors)
      ),
      samples = purrr::pmap(.l = list(d = dirichlet_params,
                                      g_A = gamma_params_A,
                                      g_B = gamma_params_B),
                            .f = ~ (rdirichlet(n_samples,
                                    shape1 = d$alpha_1,
                                    shape2 = d$beta) /
                                   rgamma(n_samples,
                                          shape = g_A$k,
                                          scale = g_A$theta)) +
                                  (rdirichlet(n_samples,
                                              shape1 = d$alpha_B,
                                              shape2 = d$beta) /
                                   rgamma(n_samples,
                                          shape = g_B$k,
                                          scale = g_B$theta))

      )
    )
}
