#' Calculate Total CM
#'
#' @param rev_per_click vector of rev per click samples
#' @param cost_per_click vector of cost per click (cpc) samples
#' @param expected_clicks vector of expected clicks (expected CTR * fixed impressions)
#'
#' @return vector of CM estimates (dbl)
#'
calculate_total_cm <- function(rev_per_click, cost_per_click, expected_clicks){
  (rev_per_click - cost_per_click) * expected_clicks
}
