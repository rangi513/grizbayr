#' Validate Priors
#'
#' @param priors
#' @param valid_priors
#' @param default_priors
#'
#' @return
#' @export
#'
#' @examples
validate_priors <- function(priors, valid_priors, default_priors) {
  are_priors_valid <- purrr::map(valid_priors, is_prior_valid(priors, .x))
  if (all(are_priors_valid)) {
    as.list(priors[valid_priors])
  } else{
    message("Using default priors.")
    default_priors
  }
}
