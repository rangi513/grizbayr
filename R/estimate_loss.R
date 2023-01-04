#' Estimate Loss
#'
#' @param posterior_samples Tibble: returned from sample_from_posterior with 3 columns
#'   `option_name`, `samples`, and `sample_id`.
#' @param distribution String: the name of the distribution
#' @param wrt_option String: the option loss is calculated with respect to (wrt). If NULL, the best option will be chosen.
#' @param metric String: the type of loss.
#'   absolute will be the difference, on the outcome scale. 0 when best = wrt_option
#'   lift will be the (best - wrt_option) / wrt_option, 0 when best = wrt_option
#'   relative_risk will be the ratio best/wrt_option, 1 when best = wrt_option
#'
#' @return numeric, the loss distribution
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr use_series %>%
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' # Requires posterior_samples dataframe. See `sample_from_posterior()`
#' # for an example.
#'
#' \dontrun{
#' estimate_loss(posterior_samples = posterior_samples, distribution = "conversion_rate")
#' }
estimate_loss <- function(posterior_samples, distribution, wrt_option = NULL, metric = c("absolute", "lift", "relative_risk")) {
  metric <- match.arg(metric)

  # estimate 'best' option if no wrt option is provided
  if (is.null(wrt_option)) {
    wrt_option <- find_best_option(posterior_samples, distribution)
  } else {
    validate_wrt_option(wrt_option, posterior_samples)
  }

  posterior_samples_wide <- posterior_samples %>%
    tidyr::pivot_wider(names_from = "option_name", values_from = "samples") %>%
    dplyr::select(-"sample_id")

  theta_star <- posterior_samples_wide[[wrt_option]]

  # Need to Flip Loss Distributions if Lower is Better
  if(is_winner_max(distribution)){
    theta_max <- as.matrix(posterior_samples_wide) %>%
      apply(1, max)

    loss_distribution <- switch(metric,
                                absolute = theta_max - theta_star,
                                lift = (theta_max - theta_star) / theta_star,
                                relative_risk = theta_max / theta_star
    )
  }else{
    theta_min <- as.matrix(posterior_samples_wide) %>%
      apply(1, min)

    loss_distribution <- switch(metric,
                                absolute = theta_star - theta_min,
                                lift = ( theta_star - theta_min ) / theta_star,
                                relative_risk = theta_min / theta_star
    )
  }
  loss_distribution
}
