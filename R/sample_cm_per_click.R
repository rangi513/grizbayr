#' Sample CM Per Click
#'
#' @description
#' Adds 4 new nested columns to the input_df: `beta_params`,
#'     `gamma_params_rev`, `gamma_params_cost`and `samples`
#'
#' @details
#'`beta_params` and `gamma_params_rev` in each row should be a
#' tibble of length 2 (\eqn{\alpha} and \eqn{\beta} parameters
#' and \eqn{k} and \eqn{\theta} parameters)
#'`samples` in each row should be a tibble of length `n_samples`
#'
#' See update_rules vignette for a mathematical representation.
#' \deqn{CMPerClick = ConversionsPerClick * RevPerConversion - CostPerClick}
#'
#' @param input_df Dataframe containing option_name (str), sum_conversions (dbl), sum_revenue (dbl),
#'     and sum_clicks (dbl).
#' @param priors Optional list of priors {alpha0, beta0} for Beta, {k0, theta0} for Gamma Inverse Revenue,
#'     and {k01, theta01} for Gamma Cost (uses alternate priors so they can be different from Revenue).
#'     Default \eqn{Beta(1,1)} and \eqn{Gamma(1, 250)} will be use otherwise.
#' @param n_samples Optional integer value. Defaults to 50,000 samples.
#'
#' @importFrom purrr pmap map2
#' @importFrom dplyr mutate %>%
#' @importFrom stats rbeta rgamma
#'
#' @return input_df with 4 new nested columns `beta_params`, `gamma_params_rev`,
#'     `gamma_params_cost`, and `samples`
#'
sample_cm_per_click <- function(input_df, priors, n_samples = 5e4){
  input_df %>%
    dplyr::mutate(
      beta_params = purrr::map2(.x = sum_conversions,
                                .y = sum_clicks,
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
      samples = purrr::pmap(list(beta_params,
                                 gamma_params_rev,
                                 gamma_params_cost),
                            ~ ( # Rev Per Click
                              stats::rbeta(n_samples,
                                           shape1 = ..1$alpha,
                                           shape2 = ..1$beta) /
                                stats::rgamma(n_samples,
                                              shape = ..2$k,
                                              scale = ..2$theta) ) -
                              # Minus Variable Cost Per Click
                              stats::rgamma(n_samples,
                                            shape = ..3$k,
                                            scale = ..3$theta)
      )
    )
}
