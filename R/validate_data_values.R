#' Validate Data Values
#'
#' Validates data values are all greater than 0.
#'
#' @param data_values List of named data values
#'
#' @return None
#'
#' @importFrom purrr walk
#'
validate_data_values <- function(data_values){
  if (length(data_values) == 0) {
    stop("No Data Values available to validate.")
  }
  purrr::walk(data_values, ~ if (.x < 0) {
    stop(paste(names(which(data_values == .x)),
               "is less than 0. Cannot update distribution."))
  })
}
