#' Is Prior Valid
#'
#' Checks if a single valid prior name is in the list of prior values and if
#'     that prior value from the list is greater than 0.
#'
#' @param priors_list A list of valid priors
#' @param valid_prior A character string
#'
#' @return Boolean (TRUE/FALSE)
#' @export
#'
#' @examples
#' example_prior_list <- list(x = 1, y = 10, z = 15)
#' is_prior_valid(priors_list = example_prior_list, valid_prior = "y")
#'
is_prior_valid <- function(priors_list, valid_prior) {
  if (length(priors_list) == 0) {
    return(FALSE)
  }
  if (!valid_prior %in% names(priors_list)) {
    warning(paste(valid_prior, "is not in priors list."))
    return(FALSE)
  }
  if (priors_list[[valid_prior]] <= 0) {
    warning(paste(valid_prior, "prior is not greater than 0."))
    return(FALSE)
  }
  TRUE
}
