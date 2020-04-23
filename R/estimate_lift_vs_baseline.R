#' Estimate Lift vs Baseline
#'
#' @param input_df Dataframe containing option_name (str) and various other columns
#'     depending on the distribution type. See vignette for more details.
#' @param distribution String of the distribution name
#' @param priors Optional list of priors. Defaults will be use otherwise.
#' @param wrt_option string the option loss is calculated with respect to (wrt). Required.
#' @param metric string the type of loss.
#'   absolute will be the difference, on the outcome scale. 0 when best = wrt_option
#'   lift will be the (best - wrt_option) / wrt_option, 0 when best = wrt_option
#'   relative_risk will be the ratio best/wrt_option, 1 when best = wrt_option
#' @param threshold Lift percentage threshold between 0 and 1. (0.7
#'   threshold is "at least 70\% lift"). Defaults to 0.7.
#'
#' @return numeric value remaining at the specified threshold
#' @export
#'
#' @examples
#' input_df <- tibble::tibble(option_name = c("A", "B", "C"),
#'     sum_clicks = c(1000, 1000, 1000),
#'     sum_conversions = c(100, 120, 110))
#' estimate_lift_vs_baseline(input_df, distribution = "conversion_rate", wrt_option_lift = "A")
#'
estimate_lift_vs_baseline <- function(input_df, distribution, priors = list(),
                                      wrt_option, metric = "lift", threshold = 0.7){
  validate_input_df(input_df, distribution)

  # Sample from posterior distribution
  posterior_samples <- sample_from_posterior(input_df, distribution, priors)

  # Calculate Lift Distribution
  estimate_lift(posterior_samples = posterior_samples,
                distribution = distribution,
                wrt_option = wrt_option,
                metric = metric) %>%
    # Select a single point
    quantile(probs = 1 - threshold)
}
