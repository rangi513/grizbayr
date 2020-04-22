#' Validate Posterior Samples Dataframe
#'
#' Function fails if posterior is not shaped correctly.
#'
#' @param posterior_samples Tibble of data in long form with 2 columns
#' `option_name` and `samples`
#'
#' @return None
#'
validate_posterior_samples <- function(posterior_samples){
  if(!is.data.frame(posterior_samples)){
    stop("Posterior samples input is not a dataframe object.")
  }
  required_columns <- c("option_name", "samples", "sample_id")
  purrr::walk(required_columns, ~validate_input_column(.x, posterior_samples, greater_than_zero = FALSE))
}
