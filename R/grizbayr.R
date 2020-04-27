#' \code{grizbayr} package
#'
#' Grizzly Bear - Bayesian Inference Package for A|B and Bandit Marketing Tests
#'
#' See the README on
#' \href{https://github.com/rangi513/grizbayr/blob/master/README.md}{GitHub}
#' or the `intro` vignette contained in the package.
#'
#' @docType package
#' @name grizbayr
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
