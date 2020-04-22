#' Calculate Multi Rev Per Session
#'
#' @param conv_rates Dirichlet samples containing a tibble with columns alpha_1, alpha_2, and alpha_0
#' @param inverse_rev_A Vector of inverse revenue samples from A conversion type
#' @param inverse_rev_B Vector of inverse revenue samples from B conversion type
#'
#' @return Vector of samples (dbl)
#'
calculate_multi_rev_per_session <- function(conv_rates, inverse_rev_A, inverse_rev_B){
  (conv_rates$alpha_1 / inverse_rev_A) + (conv_rates$alpha_2 / inverse_rev_B)
}
