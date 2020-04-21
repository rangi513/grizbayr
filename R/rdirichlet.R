#' Random Dirichlet
#'
#' Randomly samples a vector of length n from a dirichlet distribution parameterized by a vector of alphas
#' PDF of Gamma with scale = 1 : f(x)= 1/(Gamma(a)) x^(a-1) e^-(x)
#'
#' @param n integer, the number of samples
#' @param alphas paramaters of the dirichlet, interpreted as the number of success of each outcome (integer vector)
#'
#' @importFrom magrittr set_colnames %>%
#' @importFrom tibble as_tibble
#'
#' @return n x length(alphas) named tibble representing the probability of observing each outcome
#'
rdirichlet <- function(n, alphas_list) {
  alphas <- unlist(alphas_list)
  dimensions <- length(alphas)

  # generate a n x length_alphas matrix of samples from a gamma with shape = alpha_i & scale = 1
  gamma_samples <- matrix(rgamma(n * dimensions, alphas), ncol = dimensions, byrow = TRUE) %>%
    magrittr::set_colnames(names(alphas))

  # standardize each sample, so that each of n samples is a simplex
  scaled_samples <- gamma_samples / apply(gamma_samples, 1, sum)
  tibble::as_tibble(scaled_samples)
}
