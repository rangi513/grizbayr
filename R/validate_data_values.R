#' Validate Data Values
#'
#' @param data_values List of named data values
#'
#' @return None
#'
#' @importFrom purrr walk
#'
#' @examples
#' validate_data_values(data_values = list(successes = 22, failures = 100))
#' \donttest{
#' validate_data_values(data_values = list(successes = 22, failures = -1))
#' }
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
