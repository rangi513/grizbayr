#' Sample Page Views Per Session (Visit)
#'
#' Adds 2 new nested columns to the input_df: `gamma_params` and `samples`
#'     `gamma_params` in each row should be a tibble of length 2 (\eqn{k}
#'         and \eqn{\theta} parameters)
#'     `samples` in each row should be a tibble of length `n_samples`
#'
#' See update_rules vignette for a mathematical representation.
#' \deqn{page_views_i ~ Poisson(\lambda)}
#' \deqn{\lambda ~ Gamma(k, \theta)}
#' Page Views Per Visit is sampled from a Gamma distribution with a Poisson likelihood
#' of an individual having n page views by the end of their session.
#'
#' This is not always the case, so verify your data follows the shape of
#' an Poisson distribution before using this.
#'
#' @param input_df Dataframe containing option_name (str),
#'     sum_sessions (dbl), and sum_page_views_per_session (dbl).
#' @param priors Optional list of priors k0 and theta0.
#'     Default \eqn{Gamma(1, 250)} will be use otherwise.
#'     \eqn{Gamma(1, 1)} might also be a good choice for this distribution
#'     if you only have a few page views per session.
#' @param n_samples Optional integer value. Defaults to 50,000 samples.
#'
#' @importFrom purrr map map2
#' @importFrom dplyr mutate %>%
#' @importFrom rlang .data
#'
#' @return input_df with 2 new nested columns `gamma_params` and `samples`
#'
sample_page_views_per_session <- function(input_df, priors, n_samples = 5e4){
  input_df %>%
    dplyr::mutate(
      gamma_params = purrr::map2(.x = .data$sum_page_views,
                                 .y = .data$sum_sessions,
                                 ~ update_gamma(k = .x - .y, # Page Views offset since every session starts with 1 PV. Add 1 to likelihood observations
                                                theta = .y,
                                                priors = priors)
      ),
      samples = purrr::map(.x = .data$gamma_params,
                           ~ stats::rgamma(n_samples,
                                           shape = .x$k,
                                           scale = .x$theta) + 1 # Adding the offset from above back in
      )
    )
}
