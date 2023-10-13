#' Print
#'
#' @description Print results from principal stratification regression.
#' @param x An object of class \code{pstratreg}, such as the output of a call to \code{pstratreg()}
#' @param ... Other arguments to \code{print} commands
#' @returns Nothing. Prints summary of object
#' @export
#'

print.pstratreg <- function(x,...) {
  if (!methods::is(x,"pstratreg")) {
    stop("x must be a pstratreg object")
  }

  # Initialize some objects for non-standard evaluation
  mhat1 <- mhat0 <- effect_m <- NULL

  cat("Effect on mediator, where mediator indicates whether outcome will be valid\n")
  print(
    x$estimates_m |>
      dplyr::select(tidyselect::any_of(c(x$call$group_vars)),
                    mhat1,mhat0,effect_m)
  )
  cat("\nEffect on outcome among those who would have a valid outcome regardless of treatment\n")
  print(
    x$estimates_y |>
      dplyr::select(tidyselect::any_of(x$call$group_vars),
                    tidyselect::starts_with("effect_y"))
  )
}
