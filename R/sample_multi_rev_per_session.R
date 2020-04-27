#' Sample Multiple Revenue Per Session
#'
#' Adds 5 new nested columns to the input_df: `dirichlet_params`,
#'     `gamma_params_A`, `gamma_params_B`, and `samples`.
#'     This samples from multiple revenue per session distributions
#'     at once.
#'
#'
#' See update_rules vignette for a mathematical representation.
#'
#' \deqn{conversion_i ~ MultiNomial(\phi_1, \phi_2, ..., \phi_k)}
#' \deqn{\phi_k ~ Dirichlet(\alpha, \beta)}
#' Conversion Rate is sampled from a Dirichlet distribution with a Multinomial likelihood
#' of an individual converting.
#'
#' @param input_df Dataframe containing option_name (str),
#'     sum_conversions (dbl), sum_sessions (dbl), sum_revenue (dbl),
#'     sum_conversion_2 (dbl), sum_sessions_2 (dbl), sum_revenue_2 (dbl).
#' @param priors Optional list of priors alpha0 and beta0.
#'     Default \eqn{Beta(1,1)} will be use otherwise.
#' @param n_samples Optional integer value. Defaults to 50,000 samples.
#'
#' @importFrom purrr map map2 pmap
#' @importFrom dplyr mutate select %>%
#' @importFrom stats rgamma
#' @importFrom rlang .data
#'
#' @return input_df with 4 new nested columns `dirichlet_params`,
#'     `gamma_params_A`, `gamma_params_B`, and `samples`.
#'     `samples` in each row should be a tibble of length `n_samples`.
#'
sample_multi_rev_per_session <- function(input_df, priors, n_samples = 5e4){
  input_df %>%
    dplyr::mutate(
      no_clicks = .data$sum_sessions - .data$sum_conversions - .data$sum_conversions_2,
      dirichlet_params = purrr::pmap(.l = list(alpha_0 = .data$no_clicks,
                                               alpha_1 = .data$sum_conversions,
                                               alpha_2 = .data$sum_conversions_2),
                                  ~ update_dirichlet(...,
                                                     priors = priors)
      ),
      gamma_params_A = purrr::map2(.x = .data$sum_conversions,
                                   .y = .data$sum_revenue,
                                   ~ update_gamma(k = .x,
                                                  theta = .y,
                                                  priors = priors)
      ),
      gamma_params_B = purrr::map2(.x = .data$sum_conversions_2,
                                   .y = .data$sum_revenue_2,
                                   ~ update_gamma(k = .x,
                                                  theta = .y,
                                                  priors = priors)
      ),
      dirichlet_samples = purrr::map(.data$dirichlet_params,
                                     ~ rdirichlet(n_samples, alphas_list = .x)
      ),
      gamma_samples_A = purrr::map(.data$gamma_params_A,
                                   ~ stats::rgamma(n_samples,
                                            shape = .x$k,
                                            scale = .x$theta)
      ),
      gamma_samples_B = purrr::map(.data$gamma_params_B,
                                   ~ stats::rgamma(n_samples,
                                            shape = .x$k,
                                            scale = .x$theta)
      ),
      samples = purrr::pmap(.l = list(conv_rates = .data$dirichlet_samples,
                                      inverse_rev_A = .data$gamma_samples_A,
                                      inverse_rev_B = .data$gamma_samples_B),
                            ~ calculate_multi_rev_per_session(...)
      )
    ) %>%
    select(
      .data$option_name,
      .data$sum_sessions,
      .data$sum_conversions,
      .data$sum_conversions_2,
      .data$sum_revenue,
      .data$sum_revenue_2,
      .data$dirichlet_params,
      .data$gamma_params_A,
      .data$gamma_params_B,
      .data$samples
    )
}
