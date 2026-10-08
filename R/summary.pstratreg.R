#' Summary
#'
#' @description Summarize results from principal stratification regression.
#' @param object An object of class \code{pstratreg}, such as the output of a call to \code{pstratreg()}
#' @param ... Other arguments to \code{summary} commands
#' @method summary pstratreg
#' @export
#'

summary.pstratreg <- function(object,...) {
  if (!methods::is(object,"pstratreg")) {
    stop("object must be a pstratreg object")
  }
  print(object)
}
