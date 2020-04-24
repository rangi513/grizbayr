#' Update Dirichlet Distribution
#'
#' This function updates the Dirichlet distribution with the
#' Dirichlet-Multinomial conjugate prior update rule.
#'
#' TODO: This function currently only works in 3 dimensions.
#' Should be extended into N dimensions in the future. Can use ... notation.
#'
#' @param alpha_0 Double value for alpha_0 (count of failures). Must be 0 or greater.
#' @param alpha_1 Double value for alpha_1 (count of successes side 1). Must be 0 or greater.
#' @param alpha_2 Double value for alpha_2 (count of successes side 2). Must be 0 or greater.
#' @param priors An optional list object that contains alpha00, alpha01, and alpha02.
#'     Otherwise the function with use \eqn{Dirichlet(1,1,1)} as the prior distribution.
#'
#' @return tibble with columns alpha_0, alpha_1, and alpha_2
#' @export
#' @importFrom tibble tibble
#'
#' @examples
#' update_dirichlet(alpha_0 = 20, alpha_1 = 5, alpha_2 = 2)
#' sample_priors_list <- list(alpha00 = 2, alpha01 = 3, alpha02 = 5)
#' update_dirichlet(alpha_0 = 20, alpha_1 = 5, alpha_2 = 2, priors = sample_priors_list)
#'
update_dirichlet <- function(alpha_0, alpha_1, alpha_2, priors = list()) {
  validate_data_values(data_values = list(alpha_0 = alpha_0, # None
                                          alpha_1 = alpha_1, # sum_conversions
                                          alpha_2 = alpha_2)) # sum_conversions_2

  # Set Attributes
  valid_dirichlet_params <- c("alpha00", "alpha01", "alpha02")
  default_dirichlet_priors <- list(alpha00 = 1, alpha01 = 1, alpha02 = 1)

  # Validate Priors
  validated_priors <- validate_priors(priors = priors,
                                      valid_priors = valid_dirichlet_params,
                                      default_priors = default_dirichlet_priors)
  alpha00 <- validated_priors$alpha00
  alpha01 <- validated_priors$alpha01
  alpha02 <- validated_priors$alpha02

  tibble::tibble(alpha_0 = alpha_0 + alpha00,
                 alpha_1 = alpha_1 + alpha01,
                 alpha_2 = alpha_2 + alpha02)
}
