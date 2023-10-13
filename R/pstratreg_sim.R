#' Simulate Data
#'
#' @description Simulate data to illustrate principal stratification regression bounding.
#' @param n Integer number of cases
#' @returns Simulated data frame
#' @export
#' @examples
#' pstratreg_sim(n = 100)
#'

pstratreg_sim <- function(n = 100) {

  # Initialize some objects for non-standard evaluation
  x <- a <- m <- y <- NULL

  data.frame(x = stats::rnorm(n)) |>
    dplyr::mutate(a = as.logical(stats::rbinom(dplyr::n(), 1, stats::plogis(x))),
                  m = as.logical(stats::rbinom(dplyr::n(), 1, stats::plogis(1 + x + a))),
                  y = dplyr::case_when(m == 1 ~ stats::rnorm(dplyr::n(), x + a)))
}
