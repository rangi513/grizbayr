#' Validate Input DataFrame
#'
#' Validates the input dataframe has the correct type, correct required column names,
#' that the distribution is valid, that the column types are correct, and that the
#' column values are greater than or equal to 0 when they are numeric.
#'
#' @param input_df Dataframe containing option_name (str) and various other columns
#'     depending on the distribution type. See vignette for more details.
#' @param distribution String of the distribution name
#'
#' @return Bool TRUE if all checks pass.
#'
#' @importFrom dplyr select select_if filter %>%
#' @importFrom purrr walk
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' input_df <- tibble::tibble(
#'    option_name = c("A", "B"),
#'    sum_clicks = c(1000, 1000),
#'    sum_conversions = c(100, 120)
#' )
#' validate_input_df(input_df, "conversion_rate")
#'
validate_input_df <- function(input_df, distribution){
  if(!is.data.frame(input_df)){
    stop("input_df is not of type data frame.")
  }

  valid_distribution_names <- unique(distribution_column_mapping$distribution_type)
  if(!distribution %in% valid_distribution_names){
    comma_sep_names<- paste(valid_distribution_names, collapse = ", ")
    stop(paste(distribution),
         "is an invalid distribution. Select a distribution from the following:",
         comma_sep_names)
  }

  required_column_names <- distribution_column_mapping %>%
    dplyr::filter(.data$distribution_type == distribution) %>%
    dplyr::select(-.data$distribution_type) %>%
    dplyr::select_if(~ sum(.) == 1) %>%
    colnames()

  purrr::walk(required_column_names, ~validate_input_column(.x, input_df))
  TRUE
}

