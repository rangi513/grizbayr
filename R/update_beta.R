#' Update Beta
#'
#' Updates Beta Distribution with the Beta-Bernoulli
#' conjugate prior update rule
#'
#' @param alpha Double value for alpha (count of successes). Must be 0 or greater.
#' @param beta Double value for beta (count of failures). Must be 0 or greater.
#' @param priors An optional list object that contains alpha0 and
#'     beta0. Otherwise the function with use Beta(1,1) as the prior distribution.
#'
#'
#' @return A list object that contains `alpha` and `beta`
#' @export
#'
#' @examples
#' update_beta(alpha = 1, beta = 5, priors = list(alpha0 = 2, beta0 = 2))
#' update_beta(alpha = 20000, beta = 50000)
#'
update_beta <- function(alpha, beta, priors = list()) {
  validate_data_values(data_values = list(alpha = alpha, beta = beta))

  # Set Attributes
  valid_beta_params <- c("alpha0", "beta0")
  default_beta_priors <- list(alpha0 = 1, beta0 = 1)

  # Validate Priors
  validated_priors <- validate_priors(priors = priors,
                                      valid_priors = valid_beta_params,
                                      default_priors = default_beta_priors)
  alpha0 <- validated_priors$alpha0
  beta0 <- validated_priors$beta0

  list(alpha = alpha0 + alpha, beta = beta0 + beta)
}
