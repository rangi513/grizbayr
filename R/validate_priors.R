#' Validate Priors
#'
#' Validates list of priors against a vector of valid priors and if
#'    the values are not valid, default priors are returned.
#'
#' @param priors List of named priors with double values.
#' @param valid_priors A character vector of valid prior names.
#' @param default_priors A list of default priors for the distribution.
#'
#' @return A named list of valid priors for the distribution.
#'
#' @importFrom purrr map
#' @importFrom magrittr %>%
#'
#' @examples
#' priors <- list(a = 1, b = 2, c = 3)
#' valid_priors <- c("b", "c")
#' default_priors <- list(b = 1, c = 1)
#' validate_priors(priors, valid_priors, default_priors)
#'
validate_priors <- function(priors, valid_priors, default_priors) {
  are_priors_valid <- purrr::map(valid_priors, ~ is_prior_valid(priors, .x)) %>%
    unlist()
  if (all(are_priors_valid)) {
    priors[valid_priors] %>% as.list()
  } else{
    message("Using default priors.")
    default_priors
  }
}
