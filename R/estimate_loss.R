#' Estimate Loss
#'
#' @param posteriors tibble returned by sample_rewards_... n_samples by n_options
#' @param wrt_option string the option loss is calculated with respect to (wrt). If NULL, the best option will be chosen.
#' @param metric string the type of loss.
#'   absolute will be the difference, on the outcome scale. 0 when best = wrt_option
#'   lift will be the (best - wrt_option) / wrt_option, 0 when best = wrt_option
#'   relative_risk will be the ratio best/wrt_option, 1 when best = wrt_option
#'
#' @return numeric, the loss distribution
#'
#' @importFrom dplyr filter %>%
#' @importFrom magrittr use_series
#' @importFrom tidyr pivot_wider
#'
estimate_loss <- function(posterior_samples, distribution, wrt_option = NULL, metric = c("absolute", "lift", "relative_risk")) {
  metric <- match.arg(metric)

  # estimate 'best' option if no wrt option is provided
  if (is.null(wrt_option)) {
    wp <- estimate_win_prob_given_posterior(posterior_samples = posterior_samples,
                                            winner_is_max = is_winner_max(distribution))
    wrt_option <- wp %>%
      dplyr::filter(win_prob_raw == max(win_prob_raw)) %>%
      magrittr::use_series(option_name)
  } else if(!(wrt_option %in% posterior_samples$option_name)){
    stop(paste(wrt_option, "is an invalid wrt_option. Not one of in the posterior_samples options."))
  }

  posterior_samples_wide <- posterior_samples %>%
    tidyr::pivot_wider(names_from = option_name, values_from = samples) %>%
    select(-sample_id)

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
