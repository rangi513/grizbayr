#' Is Prior Valid
#'
#' @param priors
#' @param item
#'
#' @return
#' @export
#'
#' @examples
is_prior_valid <- function(priors, item) {
  if (length(priors) == 0) {
    return(FALSE)
  }
  if (!item %in% names(priors)) {
    warning(paste(item, "is not in priors list."))
    return(FALSE)
  }
  if (priors[[item]] <= 0) {
    warning(paste(item, "prior is not greater than 0."))
    return(FALSE)
  }
  TRUE
}
