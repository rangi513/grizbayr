#' Update Gamma
#'
#' Updates Gamma Distribution with the Gamma-Exponential
#' conjugate prior update rule. Parameterized by \k and \theta (not \alpha, \beta)
#'
#' @param k Double value for \k (total revenue generating events). Must be 0 or greater.
#' @param theta Double value for \theta (sum of revenue). Must be 0 or greater.
#' @param priors An optional list object that contains k0 and
#'     theta0. Otherwise the function with use Gamma(1,250) as the prior distribution.
#'
#'
#' @return A list object that contains `k` and `theta`
#' @export
#'
#' @examples
#' update_gamma(k = 1, theta = 100, priors = list(k0 = 2, theta0 = 1000))
#' update_gamma(k = 10, theta = 200)
#'
update_gamma <- function(k, theta, priors = list()) {
  validate_data_values(data_values = list(k = k, theta = theta))

  # Set Attributes
  valid_beta_params <- c("k0", "theta0")
  default_beta_priors <- list(k0 = 1, theta0 = 250)

  # Validate Priors
  validated_priors <- validate_priors(priors = priors,
                                      valid_priors = valid_beta_params,
                                      default_priors = default_beta_priors)
  k0 <- validated_priors$k0
  theta0 <- validated_priors$theta0

  list(k = k0 + k, theta = theta0/(1 + theta0 * theta))
}
