#' Update Beta
#'
#' @param successes
#' @param failures
#' @param priors A list object that contains alpha0 and beta0
#'
#' @return
#' @export
#'
#' @examples
update_beta <- function(successes, failures, priors = list()) {
  valid_beta_params <- c("alpha0", "beta0")
  default_beta_priors <- list(alpha0 = 1, beta0 = 1)
  validated_priors <- validate_priors(priors = priors,
                                      valid_priors = valid_beta_params,
                                      default_priors = default_beta_priors)
  list(alpha = alpha0 + successes, beta = beta0 + failures)
}
