#' Estimate Lift Distribution
#'
#' Estimates lift distribution vector from posterior samples.
#'
#' @param posterior_samples Tibble returned from sample_from_posterior with 3 columns
#'   `option_name`, `samples`, and `sample_id`.
#' @param distribution String of the distribution name
#' @param wrt_option string the option lift is calculated with respect to (wrt). Required.
#' @param metric string the type of lift.
#'   absolute will be the difference, on the outcome scale. 0 when best = wrt_option
#'   lift will be the (best - wrt_option) / wrt_option, 0 when best = wrt_option
#'   relative_risk will be the ratio best/wrt_option, 1 when best = wrt_option
#'
#' @return numeric, the lift distribution
#' @export
#'
#' @examples
#' # Requires posterior_samples dataframe. See `sample_from_posterior()`
#' # for an example.
#'
#' \dontrun{
#' estimate_lift(posterior_samples = posterior_samples,
#'               distribution = "conversion_rate",
#'               wrt_option = "A",
#'               metric = "lift")
#'}
#'
estimate_lift <- function(posterior_samples, distribution, wrt_option, metric = c("absolute", "lift", "relative_risk")){
  metric <- match.arg(metric)
  validate_wrt_option(wrt_option, posterior_samples)

  best_option <- find_best_option(posterior_samples, distribution)

  # Format samples wider so matrices can be used
  posterior_samples_wide <- posterior_samples %>%
    tidyr::pivot_wider(names_from = option_name, values_from = samples) %>%
    select(-sample_id)

  theta_best <- posterior_samples_wide[[best_option]]
  theta_control <- posterior_samples_wide[[wrt_option]]

  # Need to Flip Loss Distributions if Lower is Better
  if(is_winner_max(distribution)){
    lift_distribution <- switch(metric,
                                absolute = theta_best - theta_control,
                                lift = (theta_best - theta_control) / theta_control,
                                relative_risk = theta_best / theta_control
    )
  }else{
    lift_distribution <- switch(metric,
                                absolute = theta_control - theta_best,
                                lift = ( theta_control - theta_best ) / theta_control,
                                relative_risk = theta_best / theta_control
    )
  }
  lift_distribution
}
