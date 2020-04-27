#' Is Winner Max
#'
#' Determines if the max or min function should be used for win probability.
#' If CPA or CPC distribution, lower is better, else higher number is better.
#'
#' @param distribution String: the name of the distribution
#'
#' @return Boolean TRUE/FALSE
#'
is_winner_max <- function(distribution){
  switch(distribution,
         "cpc" = ,
         "cpa" = FALSE,
         TRUE)
}
