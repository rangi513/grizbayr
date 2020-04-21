#' Sample From Posterior
#'
#' Selects which function to use to sample from the posterior distribution
#'
#' @param input_df Dataframe containing option_name (str) and various other columns
#'     depending on the distribution type. See vignette for more details.
#' @param distribution String of the distribution name
#' @param priors Optional list of priors. Defaults will be use otherwise.
#' @param n_samples Optional integer value. Defaults to 50,000 samples.
#'
#' @return A tibble with 2 columns: option_name (chr) and sample (dbl) [long form data].
#' @export
#'
#' @examples
#' input_df <- tibble::tibble(
#'    option_name = c("A", "B"),
#'    sum_clicks = c(1000, 1000),
#'    sum_conversions = c(100, 120)
#' )
#' sample_from_posterior(input_df, "conversion_rate")
#'
sample_from_posterior <- function(input_df, distribution, priors = list(), n_samples = 5e4){
  switch(distribution,
         "conversion_rate" =       sample_conv_rate(input_df, priors, n_samples),
         "response_rate" =         sample_response_rate(input_df, priors, n_samples),
         "multi_response_rate" =   sample_multi_response_rate(input_df, priors, n_samples),
         "ctr" =                   sample_ctr(input_df, priors, n_samples),
         "rev_per_session" =       sample_rev_per_session(input_df, priors, n_samples),
         "multi_rev_per_session" = sample_multi_rev_per_session(input_df, priors, n_samples),
         "cpa" =                   sample_cpa(input_df, priors, n_samples),
         "total_cm" =              sample_total_cm(input_df, priors, n_samples),
         "cm_per_click" =          sample_cm_per_click(input_df, priors, n_samples),
         "cpc" =                   sample_cpc(input_df, priors, n_samples),
         stop(
           paste(distribution,
                 "is an invalid distribution type. Select from one of the following:",
                 paste(distribution_column_mapping$distribution_type, collapse = ", "))
           )
  )
}