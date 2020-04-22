#' Update Gamma
#'
#' Updates Gamma Distribution with the Gamma-Exponential
#' conjugate prior update rule. Parameterized by \eqn{k} and \eqn{\theta} (not \eqn{\alpha, \beta})
#'
#' @param k Double value for \eqn{k} (total revenue generating events). Must be 0 or greater.
#' @param theta Double value for \eqn{\theta} (sum of revenue). Must be 0 or greater.
#' @param priors An optional list object that contains k0 and
#'     theta0. Otherwise the function will use \eqn{Gamma(1,250)} as the prior distribution.
#'     If a second gamma distribution is used k01 and theta01 can be defined as separate priors
#'     when alternate_priors is set to TRUE.
#' @param alternate_priors Boolean Defaults to FALSE. Allows a user to specify alternate
#'     prior names so the same prior isn't required when multiple gamma distributions are used.
#'
#' @return A list object that contains `k` and `theta`
#' @export
#' @importFrom tibble tibble
#' @importFrom magrittr set_names
#'
#' @examples
#' update_gamma(k = 1, theta = 100, priors = list(k0 = 2, theta0 = 1000))
#' update_gamma(k = 10, theta = 200)
#'
update_gamma <- function(k, theta, priors = list(), alternate_priors = FALSE) {
  validate_data_values(data_values = list(k = k, theta = theta))

  # Set Attributes
  valid_gamma_params <- if(alternate_priors) c("k01", "theta01") else c("k0", "theta0")
  default_gamma_priors <- list(k0 = 1, theta0 = 250) %>%
    magrittr::set_names(valid_gamma_params)

  # Validate Priors
  validated_priors <- validate_priors(priors = priors,
                                      valid_priors = valid_gamma_params,
                                      default_priors = default_gamma_priors)

  k0 <- if(alternate_priors) validated_priors$k01 else validated_priors$k0
  theta0 <- if(alternate_priors) validated_priors$theta01 else validated_priors$theta0

  tibble::tibble(k = k0 + k, theta = theta0/(1 + theta0 * theta))
}
