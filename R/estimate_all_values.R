#' Estimate All Values
#'
#' Efficiently estimates all values at once so the posterior only need to be
#' sampled one time. This function will return as a list win probability,
#' value remaining, estimated percent lift with respect to the provided option,
#' and the win probabilty of the best option vs the provided option.
#'
#' TODO: Add high density credible intervals to this output for each option.
#'
#' @param input_df Dataframe containing option_name (str) and various other columns
#'     depending on the distribution type. See vignette for more details.
#' @param distribution String of the distribution name
#' @param wrt_option_lift String: the option lift and win probability is calculated
#'     with respect to (wrt). Required.
#' @param priors Optional list of priors. Defaults will be use otherwise.
#' @param wrt_option_vr String: the option against which loss (value remaining)
#'   is calculated. If NULL the best option will be used. (optional)
#' @param loss_threshold The confidence interval specifying what the "worst case scenario" should be.
#'     Defaults to 95%. (optional)
#' @param lift_threshold The confidence interval specifying how likely the lift is to be true.
#'     Defaults to 70%. (optional)
#' @param metric string the type of loss.
#'   absolute will be the difference, on the outcome scale. 0 when best = wrt_option
#'   lift will be the (best - wrt_option) / wrt_option, 0 when best = wrt_option
#'   relative_risk will be the ratio best/wrt_option, 1 when best = wrt_option
#'
#'
#' @return A list with 4 named items: Win Probability, Value Remaining,
#'     Lift vs Baseline, and Win Probability vs Baseline.
#' @export
#'
estimate_all_values <- function(input_df, distribution, wrt_option_lift, priors = list(),
                                wrt_option_vr = NULL, loss_threshold = 0.95, lift_threshold = 0.7,
                                metric = "lift"){
  validate_input_df(input_df, distribution)

  # Sample from posterior distribution
  posterior_samples <- sample_from_posterior(input_df, distribution, priors)

  # Calculate Win Probability
  win_prob <- estimate_win_prob_given_posterior(posterior_samples = posterior_samples,
                                                winner_is_max = is_winner_max(distribution))

  # Calculate Value Remaining
  vr <- estimate_loss(posterior_samples = posterior_samples,
                      distribution = distribution,
                      wrt_option = wrt_option_vr,
                      metric = metric) %>%
    quantile(probs = loss_threshold)

  # Calculate Lift Relative to Baseline
  lift <- estimate_lift(posterior_samples = posterior_samples,
                        distribution = distribution,
                        wrt_option = wrt_option_lift,
                        metric = metric) %>%
    quantile(probs = 1 - lift_threshold)

  # Calculate Win Prob vs Baseline
  win_prob_vs_base <- estimate_win_prob_vs_baseline_given_posterior(posterior_samples,
                                                                    distribution,
                                                                    wrt_option_lift)
  list(`Win Probability` = win_prob,
       `Value Remaining` = vr,
       `Lift vs Baseline` = lift,
       `Win Probability vs Baseline` = win_prob_vs_base)
}
