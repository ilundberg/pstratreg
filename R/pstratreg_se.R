#' Standard errors for pstratreg
#'
#' @description Calculate standard errors by a nonparametric bootstrap, and 95% confidence intervals by a Normal approximation
#' @param x An object of class \code{pstratreg}, such as the output of a call to \code{pstratreg()}
#' @param r Number of bootstrap replicates
#' @returns Data frame with estimates, standard errors, and confidence intervals for many quantities of interest.
#' @export
#' @examples
#' sim <- pstratreg_sim(n = 100)
#' result <- pstratreg(
#'      formula_y = formula(y ~ x + a),
#'      formula_m = formula(m ~ x + a),
#'      data = sim,
#'      treatment_name = "a"
#' )
#' se <- pstratreg_se(result, r = 10)

pstratreg_se <- function(
    x,
    r = 100
) {
  if (!methods::is(x,"pstratreg")) {
    stop("Input must be a pstratreg object")
  }
  if (!methods::is(r,"numeric")) {
    stop("r must be a number of bootstrap replicates")
  }

  # Initialize objects for nonstandard evaluation
  estimate <- estimand <- NULL

  x_args <- x$call
  if (is.null(x_args$group_vars)) {
    point <- x$estimates_m |>
      dplyr::bind_cols(x$estimates_y) |>
      tidyr::pivot_longer(cols = tidyselect::everything(),
                          names_to = "estimand",
                          values_to = "estimate")
  } else {
    point <- x$estimates_m |>
      dplyr::left_join(x$estimates_y, by = x_args$group_vars) |>
      tidyr::pivot_longer(cols = -tidyselect::all_of(x_args$group_vars),
                          names_to = "estimand",
                          values_to = "estimate")
  }

  bs <- foreach::foreach(rep = 1:r, .combine = "rbind") %do% {
    xstar_args <- x_args
    chosen <- sample(1:nrow(x_args$data), replace = T)
    xstar_args$data <- xstar_args$data[chosen,]
    if (!is.null(xstar_args$weights)) {
      xstar_args$weights <- xstar_args$weights[chosen]
    }
    result_star <- suppressWarnings(do.call(pstratreg, args = xstar_args))

    if (is.null(x_args$group_vars)) {
      point_star <- result_star$estimates_m |>
        dplyr::bind_cols(result_star$estimates_y) |>
        tidyr::pivot_longer(cols = tidyselect::everything(),
                            names_to = "estimand",
                            values_to = "estimate")
    } else {
      point_star <- result_star$estimates_m |>
        dplyr::left_join(result_star$estimates_y, by = x_args$group_vars) |>
        tidyr::pivot_longer(cols = -tidyselect::all_of(x_args$group_vars),
                            names_to = "estimand",
                            values_to = "estimate")
    }
    return(point_star)
  }
  se <- bs |>
    dplyr::group_by_at(c("estimand",x_args$group_vars)) |>
    dplyr::summarize(se = stats::sd(estimate),
                     .groups = "drop")

  result <- point |>
    dplyr::left_join(se, by = c("estimand",x_args$group_vars)) |>
    dplyr::mutate(ci.min = estimate - stats::qnorm(.975) * se,
                  ci.max = estimate + stats::qnorm(.975) * se) |>
    dplyr::arrange(estimand)
  return(result)
}
