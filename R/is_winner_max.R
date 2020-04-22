#' Is Winner Max
#'
#' Determines if the max or min function should be used for win probability.
#' If CPA or CPC distribution, lower is better, else higher number is better.
#'
#' @param is_winner_max
#'
#' @return Boolean TRUE/FALSE
#'
is_winner_max <- function(distribution){
  switch(distribution,
         "cpc" = ,
         "cpa" = FALSE,
         TRUE)
}
