#' Validate With Respect To Option
#'
#' Verify that the option provided is in the poster_samples dataframe `option_name` column.
#' Raises error if not TRUE
#'
#' @param wrt_option string name of the option
#' @param posterior_samples Tibble returned from sample_from_posterior with 3 columns
#'   `option_name`, `samples`, and `sample_id`.
#'
#' @return None
validate_wrt_option <- function(wrt_option, posterior_samples){
  if(!(wrt_option %in% posterior_samples$option_name)){
    stop(paste(wrt_option, "is an invalid wrt_option. Not one of in the posterior_samples options."))
  }
}
