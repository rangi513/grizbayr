#' Sample Conversion Rate
#'
#' Adds 2 new nested columns to the input_df: `beta_params` and `samples`
#'     `beta_params` in each row should be a tibble of length 2 (\eqn{\alpha}
#'         and \eqn{\beta} parameters)
#'     `samples` in each row should be a tibble of length `n_samples`
#'
#' @param input_df Dataframe containing option_name (str), sum_conversions (dbl),
#'     and sum_clicks (dbl).
#' @param priors Optional list of priors alpha0 and beta0. Default \eqn{Beta(1,1)} will be use otherwise.
#' @param n_samples Optional integer value. Defaults to 50,000 samples.
#'
#' @importFrom purrr map map2
#' @importFrom dplyr mutate
#'
#' @return input_df with 2 new nested columns `beta_params` and `samples`
#'
sample_conv_rate <- function(input_df, priors, n_samples = 5e4){
  input_df %>%
    dplyr::mutate(
      beta_params = purrr::map2(.x = sum_conversions,
                                .y = sum_clicks,
                                ~ update_beta(alpha = .x,
                                              beta = .y - .x,
                                              priors = priors)
                                ),
      samples = purrr::map(.x = beta_params,
                           ~ rbeta(n_samples,
                                   shape1 = .x$alpha,
                                   shape2 = .x$beta)
                           )
    )
}