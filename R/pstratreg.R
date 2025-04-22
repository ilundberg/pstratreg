#' Estimate Principal Stratification Regression Bounds
#'
#' @description Uses principal stratification and parametric models to bound the average causal effect among those who would have a valid outcome under either treatment condition
#' @param formula_y A model formula for the outcome
#' @param formula_m A model formula for the binary indicator for whether the outcome exists
#' @param formula_sq_resid A model formula for the squared residuals for a variance function regression. Should only use variables that are used in \code{formula_y}. Because the Gamma regression for squared residuals is more computationally demanding, one might want a simpler \code{formula_sq_resid} than \code{formula_y}, perhaps to allow only highly structured heteroskedasticity. Defaults to match \code{formula_y}. No outcome need be specified in this formula.
#' @param family_y Character family of the outcome, either \code{"gaussian"} or \code{"binomial"}
#' @param homoskedastic A logical for whether homoskedasticity of the outcome should be assumed. If \code{FALSE}, estimates by variance function regression for log squared residuals with the same formula as in \code{formula_y}.
#' @param data A data frame
#' @param weights A numeric vector of weights, of length \code{nrow(data)}
#' @param treatment_name A character for the name of the treatment in \code{data},
#' @param monotonicity_positive A logical. Whether to assume M1 >= M0, so that treatment never causes the outcome to be undefined
#' @param monotonicity_negative A logical. Whether to assume M1 <= M0, so that lack of treatment never causes the outcome to be undefined
#' @param mean_dominance_y1_positive A logical. Whether to assume E(Y1 | S1 = 1, S0 = 1) >= E(Y1 | S1 = 1, S0 = 0), so that the always-survivor treated units have mean outcomes at least as great as the observed-survivor treated units
#' @param mean_dominance_y1_negative A logical. Whether to assume E(Y1 | S1 = 1, S0 = 1) <= E(Y1 | S1 = 1, S0 = 0), so that the always-survivor treated units have mean outcomes no greater the observed-survivor treated units
#' @param mean_dominance_y0_positive A logical. Whether to assume E(Y0 | S1 = 1, S0 = 1) >= E(Y0 | S1 = 0, S0 = 1), so that the always-survivor untreated units have mean outcomes at least as great as the observed-survivor untreated units
#' @param mean_dominance_y0_negative A logical. Whether to assume E(Y0 | S1 = 1, S0 = 1) <= E(Y0 | S1 = 0, S0 = 1), so that the always-survivor untreated units have mean outcomes no greater the observed-survivor untreated units
#' @param aggregate A logical. Whether to aggregate results or return one estimate per row of \code{data}
#' @param group_vars Character vector of names of variables in \code{data} by which to group when aggregating results. Only relevant when \code{aggregate = TRUE}
#' @returns An object of class \code{pstratreg}
#' @export
#' @examples
#' sim <- pstratreg_sim(n = 100)
#' result <- pstratreg(
#'      formula_y = formula(y ~ x + a),
#'      formula_m = formula(m ~ x + a),
#'      data = sim,
#'      treatment_name = "a"
#' )
#' result <- pstratreg(
#'      formula_y = formula(y ~ x + a),
#'      formula_m = formula(m ~ x + a),
#'      data = sim,
#'      treatment_name = "a",
#'      aggregate = TRUE,
#'      homoskedastic = FALSE,
#'      monotonicity_negative = TRUE
#' )
#'

pstratreg <- function(
    formula_y,
    formula_m,
    formula_sq_resid = NULL,
    family_y = "gaussian", # currently only gaussian and binomial
    homoskedastic = T, # only relevant if family_y = gaussian
    data,
    weights = NULL, # numeric vector of weights, of length nrow(data)
    treatment_name,
    monotonicity_positive = FALSE, # logical. Assume M1 >= M0?
    monotonicity_negative = FALSE, # logical. Assume M1 <= M0?
    mean_dominance_y1_positive = FALSE,
    mean_dominance_y1_negative = FALSE,
    mean_dominance_y0_positive = FALSE,
    mean_dominance_y0_negative = FALSE,
    aggregate = TRUE,
    group_vars = NULL
) {

  # Save all arguments to return at the end
  call <- list(
    formula_y = formula_y,
    formula_m = formula_m,
    formula_sq_resid = formula_sq_resid,
    family_y = family_y,
    data = data,
    weights = weights,
    treatment_name = treatment_name,
    monotonicity_positive = monotonicity_positive,
    monotonicity_negative = monotonicity_negative,
    mean_dominance_y1_positive = mean_dominance_y1_positive,
    mean_dominance_y1_negative = mean_dominance_y1_negative,
    mean_dominance_y0_positive = mean_dominance_y0_positive,
    mean_dominance_y0_negative = mean_dominance_y0_negative,
    aggregate = aggregate,
    group_vars = group_vars
  )

  # Check that formula inputs are the right class
  if (!methods::is(formula_y, "formula")) {
    stop("formula_y must be a formula object")
  }
  if (!methods::is(formula_m, "formula")) {
    stop("formula_m must be a formula object")
  }
  # Check that mediator and outcome have different outcomes
  if (as.character(formula_y)[2] == as.character(formula_m)[2]) {
    stop("formula_y and formula_m should have different outcomes")
  }

  # Make residual formula have the outcome sq_resid
  if (!homoskedastic & !is.null(formula_sq_resid)) {
    formula_sq_resid_ch <- as.character(formula_sq_resid)
    # drop the outcome from the formula
    if (length(formula_sq_resid_ch) == 3) {
      formula_sq_resid_ch <- formula_sq_resid_ch[c(1,3)]
    }
    # make the outcome sq_resid
    formula_sq_resid <- stats::formula(paste0(c("sq_resid",formula_sq_resid_ch),collapse = " "))
  } else if (!homoskedastic & is.null(formula_sq_resid)) {
    formula_sq_resid <- stats::formula(paste0(c("sq_resid",as.character(formula_y)[c(1,3)]),collapse = " "))
  }

  # Extract the outcome and mediator variables
  outcome_name <- as.character(formula_y)[2]
  mediator_name <- as.character(formula_m)[2]
  treatment_name_regex <- paste0("^",treatment_name,"$")

  # Checks on family_y taken from glm() function
  if (is.character(family_y))
    family_y <- get(family_y, mode = "function", envir = parent.frame())
  if (is.function(family_y))
    family_y <- family_y()
  if (is.null(family_y$family)) {
    print(family_y)
    stop("'family_y' not recognized, must be a valid input to the family argument of glm")
  }
  if (!(family_y$family %in% c("gaussian","binomial"))) {
    stop("family_y needs to be either gaussian or binomial, where binomial should be a binary outcome")
  }

  # Check on homoskedastic
  if (!is.logical(homoskedastic)) {
    stop("homoskedastic should be a logical")
  }

  # Check data input
  if (!is.data.frame(data)) {
    stop("data should be a data frame")
  }
  if (!all(all.vars(formula_y) %in% colnames(data))) {
    stop("All variables in formula_y should be in data")
  }
  if (!all(all.vars(formula_m) %in% colnames(data))) {
    stop("All variables in formula_m should be in data")
  }

  # Add weights to data
  if (is.null(weights)) {
    data$sample_weight_variable = 1
  } else {
    data$sample_weight_variable = weights
  }

  all_vars_combined <- c(base::union(all.vars(formula_m),all.vars(formula_y)),"sample_weight_variable")
  num_missing <- colSums(is.na(data[,all_vars_combined]))
  if (!all(num_missing[names(num_missing) != outcome_name] == 0)) {
    stop("There are missing values on variables other than the outcome in formula_m or formula_y")
  }

  # Indicator of survival
  survives <- as.logical(data[[mediator_name]])
  valid_outcome <- !is.na(data[[outcome_name]])
  if (any(is.na(survives))) {
    stop("There should be no missing values on the mediator")
  }
  if (any(survives & !valid_outcome)) {
    stop("There are units who are coded as 1 or TRUE on the mediator but who have missing values on the outcome")
  }
  if (any(!survives & valid_outcome)) {
    stop("There are units who are coded 0 or FALSE on the mediator but who have non-missing values on the outcome")
  }

  # Check on treatment name
  if (!is.character(treatment_name)) {
    stop("treatment_name should be a character")
  }
  if (length(treatment_name) != 1) {
    stop("treatment_name should be a single value, not a vector")
  }
  if (!treatment_name %in% all.vars(formula_y)[-1]) {
    stop("The character value of treatment_name should be a predictor in formula_y")
  }
  if (!treatment_name %in% all.vars(formula_m)[-1]) {
    stop("The character value of treatment_name should be a predictor in formula_m")
  }

  # Check monotonocity argument validity
  if (!is.logical(monotonicity_positive)) {
    stop("monotonicity_positive should be TRUE or FALSE to indicate whether M1 >= M0 is assumed")
  }
  if (!is.logical(monotonicity_negative)) {
    stop("monotonicity_negative should be TRUE or FALSE to indicate whether M1 <= M0 is assumed")
  }
  if (monotonicity_positive & monotonicity_negative) {
    stop("monotonicity_positive and monotonicity_negative should not both be TRUE, which would entail an assumption that the treatment does not affect the mediator for any units, in which case this method is not relevant")
  }

  # Check mean dominance argument validity
  if (
    !is.logical(mean_dominance_y1_positive) |
    !is.logical(mean_dominance_y1_negative) |
    !is.logical(mean_dominance_y0_positive) |
    !is.logical(mean_dominance_y0_negative)
  ) {
    stop("mean_dominance_y._. arguments should be TRUE or FALSE to indicate whether each form of mean dominance is assumed")
  }

  # Check that aggregate is logical
  if (!is.logical(aggregate)) {
    stop("aggregate should be a logical for whether averages should be returned")
  }
  if (!is.null(group_vars)) {
    if (!is.character(group_vars)) {
      stop("group_vars should be a character vector of grouping variables within data")
    }
    if (!all(group_vars %in% colnames(data))) {
      stop("group_vars should be a character vector of grouping variables within data, but some currently do not appear in data")
    }
  }

  # Initialize some objects for non-standard evaluation
  mhat1 <- mhat0 <- effect_m <- sample_weight_variable <- p_always_survive <- value <- NULL

  # Fit a model for the mediator
  fit_m <- stats::glm(formula_m,
                      data = data,
                      family = stats::binomial,
                      weights = sample_weight_variable)

  # Fit a model for the outcome
  fit_y <- stats::glm(formula_y,
                      data = data[valid_outcome,],
                      family = family_y,
                      weights = sample_weight_variable)

  # Create data frames with treatment set to 1 and to 0
  data_d1 <- data_d0 <- data
  if (methods::is(data[[treatment_name]], "logical")) {
    data_d1[[treatment_name]] <- T
    data_d0[[treatment_name]] <- F
  } else {
    data_d1[[treatment_name]] <- 1
    data_d0[[treatment_name]] <- 0
  }

  # Probability of survival given confounders, under each treatment condition
  # the _trunc versions will be forced to comply with monotonicity
  mhat1 <- mhat1_trunc <- stats::predict(fit_m, type = "response", newdata = data_d1)
  mhat0 <- mhat0_trunc <- stats::predict(fit_m, type = "response", newdata = data_d0)

  # Calculate the probability of each stratum given X
  # with equations that depend on the monotonicy assumption
  if (!monotonicity_positive & !monotonicity_negative) {
    # Calculate lower limit on probability of always survivor.
    p_always_lower <- ifelse(
      mhat1 + mhat0 - 1 > 0,
      mhat1 + mhat0 - 1,
      0
    )

    # Calculate the upper limit on the probability of always survivor.
    p_always_upper <- ifelse(
      mhat1 < mhat0,
      mhat1,
      mhat0
    )

    # Convert to probabilities among the survivors in each treatment condition
    p_always_lower_1 <- p_always_lower / mhat1
    p_always_lower_0 <- p_always_lower / mhat0

  } else if (monotonicity_positive) {
    if (any(mhat0 > mhat1)) {
      violation_cases <- mhat0 > mhat1
      warning(paste("Monotonicity violated in",
                    round(100*mean(violation_cases),1),
                    "% of cases\nForcing mhat1_trunc = mhat0_trunc at midpoint of estimates for those"))
      midpoint <- .5 * (mhat1 + mhat0)
      mhat1_trunc[violation_cases] <-
        mhat0_trunc[violation_cases] <-
        midpoint[violation_cases]
    }
    # Under positive montonicity, probability of being always-survivor is point-identified
    p_always_lower_1 <- p_always_upper_1 <- mhat0_trunc / mhat1_trunc
    # and equals 1 among the untreated survivors
    p_always_lower_0 <- p_always_upper_0 <- mhat0_trunc / mhat0_trunc

  } else if (monotonicity_negative) {
    if (any(mhat1 > mhat0)) {
      violation_cases <- mhat1 > mhat0
      warning(paste("Monotonicity violated in",
                    round(100*mean(violation_cases),1),
                    "% of cases\nForcing mhat1_trunc = mhat0_trunc at midpoint of estimates for those"))
      midpoint <- .5 * (mhat1 + mhat0)
      mhat1_trunc[violation_cases] <-
        mhat0_trunc[violation_cases] <-
        midpoint[violation_cases]
    }
    # Under negative montonicity, probability of being always-survivor is point-identified
    p_always_lower_0 <- p_always_upper_1 <- mhat1_trunc / mhat0_trunc
    # and equals 1 among the treated survivors
    p_always_lower_1 <- p_always_upper_1 <- mhat1_trunc / mhat1_trunc
  }

  # Write a function to find for each unit
  # the probability that treated or untreated people
  # who look like them and survive are always-survivors
  # as opposed to induced one or the other way
  #p_always_for_yhat0 <- p_s$always_survive / (p_s$always_survive + p_s$defier)
  #p_induced_for_yhat0 <- 1 - p_always_for_yhat0
  #p_always_for_yhat1 <- p_s$always_survive / (p_s$always_survive + p_s$complier)
  #p_induced_for_yhat1 <- 1 - p_always_for_yhat1

  # Calculate naive predictions
  yhat0_naive <- stats::predict(fit_y, type = "response", newdata = data_d0)
  yhat1_naive <- stats::predict(fit_y, type = "response", newdata = data_d1)

  # Determine the proportion to drop
  to_drop_lower_0 <- 1 - p_always_upper_0
  to_drop_lower_1 <- 1 - p_always_upper_1
  to_drop_upper_0 <- 1 - p_always_lower_0
  to_drop_upper_1 <- 1 - p_always_lower_1

  # Create lower and upper bounds on expected value of Y
  if (family_y$family == "binomial") {

    # lower bound assumes all compliers + defiers are y = 1
    yhat0_lower <- (yhat0_naive - to_drop_upper_0) / (1 - to_drop_upper_0)
    yhat1_lower <- (yhat1_naive - to_drop_upper_1) / (1 - to_drop_upper_1)

    # upper bound assumes all compliers + defiers are y = 0
    yhat0_upper <- (yhat0_naive - to_drop_lower_0) / (1 - to_drop_lower_0)
    yhat1_upper <- (yhat1_naive - to_drop_lower_1) / (1 - to_drop_lower_1)

    # enforce lower bound not below 0 and upper bound not above 1
    yhat0_lower <- ifelse(yhat0_lower < 0, 0, yhat0_lower)
    yhat1_lower <- ifelse(yhat1_lower < 0, 0, yhat1_lower)
    yhat0_upper <- ifelse(yhat0_upper > 1, 1, yhat0_upper)
    yhat1_upper <- ifelse(yhat1_upper > 1, 1, yhat1_upper)

  } else if (family_y$family == "gaussian") {

    if (homoskedastic) {
      fit_sq_resid <- NULL
      resid_sd <- rep(stats::sd(fit_y$residuals), nrow(data))
    } else {
      # Fit a variance function regression for outcome residuals
      # See two-step estimator in Western and Bloome (2009)
      # note: the .001 is to help with numerical issues near log(0)
      # adding the .001 as here could create problems if a user had data on a very small scale
      data$sq_resid <- .001 + (data[[outcome_name]] - stats::predict(fit_y, type = "response", newdata = data)) ^ 2
      fit_sq_resid <- stats::glm(formula_sq_resid,
                                 data = data[valid_outcome,],
                                 weights = sample_weight_variable,
                                 family = stats::Gamma(link = "log"))
      resid_sd <- sqrt(stats::predict(fit_sq_resid, newdata = data, type = "response"))
    }

    # Make many draws from a standard normal residual
    std_residual_draws <- sort(stats::rnorm(10e3))

    # Make an estimator for the residual means of each case
    residual_mean_estimator <- function(prop_always, resid_sd, std_residual_draws, upper = T) {
      if (prop_always == 1) {
        return(0)
      }
      if (upper) {
        keep <- round((1 - prop_always) * length(std_residual_draws)):length(std_residual_draws)
      } else {
        keep <- 1:round(prop_always * length(std_residual_draws))
      }
      # Take the mean of the standard residual draws
      keep_mean <- mean(std_residual_draws[keep])
      # Scale by the residual standard error
      scaled <- keep_mean * resid_sd
      return(scaled)
    }

    # Loop over cases to produce residual mean estimates
    # Note that in each case, we set prop_always to the smallest possible value
    # which results in the most extreme possible estimates
    residual_means_upper_0 <- sapply(1:nrow(data), function(i) {
      residual_mean_estimator(
        prop_always = p_always_lower_0,
        resid_sd = resid_sd[i],
        std_residual_draws = std_residual_draws,
        upper = T
      )
    })
    residual_means_upper_1 <- sapply(1:nrow(data), function(i) {
      residual_mean_estimator(
        prop_always = p_always_lower_1,
        resid_sd = resid_sd[i],
        std_residual_draws = std_residual_draws,
        upper = T
      )
    })
    residual_means_lower_0 <- sapply(1:nrow(data), function(i) {
      residual_mean_estimator(
        prop_always = p_always_lower_0,
        resid_sd = resid_sd[i],
        std_residual_draws = std_residual_draws,
        upper = F
      )
    })
    residual_means_lower_1 <- sapply(1:nrow(data), function(i) {
      residual_mean_estimator(
        prop_always = p_always_lower_1,
        resid_sd = resid_sd[i],
        std_residual_draws = std_residual_draws,
        upper = F
      )
    })
    yhat0_lower <- yhat0_naive + residual_means_lower_0
    yhat0_upper <- yhat0_naive + residual_means_upper_0
    yhat1_lower <- yhat1_naive + residual_means_lower_1
    yhat1_upper <- yhat1_naive + residual_means_upper_1
  }

  # Mean dominance assumption: Narrow bounds if assumed
  if (mean_dominance_y1_positive) {
    # Assumption: E(Y^1 | S^1 = 1, S^0 = 1) >= E(Y^1 | S^1 = 1, S^0 = 0)
    # Implication: Lower bound for Y^1 becomes naive estimate
    yhat1_lower <- yhat1_naive
  }
  if (mean_dominance_y1_negative) {
    # Assumption: E(Y^1 | S^1 = 1, S^0 = 1) <= E(Y^1 | S^1 = 1, S^0 = 0)
    # Implication: Upper bound for Y^1 becomes naive estimate
    yhat1_upper <- yhat1_naive
  }
  if (mean_dominance_y0_positive) {
    # Assumption: E(Y^0 | S^1 = 1, S^0 = 1) >= E(Y^0 | S^1 = 0, S^0 = 1)
    # Implication: Lower bound for Y^0 becomes naive estimate
    yhat0_lower <- yhat0_naive
  }
  if (mean_dominance_y0_negative) {
    # Assumption: E(Y^0 | S^1 = 1, S^0 = 1) <= E(Y^0 | S^1 = 0, S^0 = 1)
    # Implication: Upper bound for Y^0 becomes naive estimate
    yhat0_upper <- yhat0_naive
  }

  # Prepare data frame of estimates for M and for Y
  estimates_m <- data |>
    dplyr::mutate(mhat0 = mhat0,
                  mhat0_trunc = mhat0_trunc,
                  mhat1 = mhat1,
                  mhat1_trunc = mhat1_trunc,
                  effect_m = mhat1 - mhat0)

  estimates_y <- data |>
    dplyr::mutate(yhat0_naive = yhat0_naive,
                  yhat0_lower = yhat0_lower,
                  yhat0_upper = yhat0_upper,
                  yhat1_naive = yhat1_naive,
                  yhat1_lower = yhat1_lower,
                  yhat1_upper = yhat1_upper,
                  effect_y_naive = yhat1_naive - yhat0_naive,
                  effect_y_lower = yhat1_lower - yhat0_upper,
                  effect_y_upper = yhat1_upper - yhat0_lower)

  # Aggregate to averages among compliers if desired
  # possibly within groups defined by group_vars
  if (aggregate) {
    estimates_m <- estimates_m |>
      dplyr::select(tidyselect::any_of(c(group_vars,"sample_weight_variable")),
                    tidyselect::starts_with("mhat"),
                    tidyselect::starts_with("effect")) |>
      tidyr::pivot_longer(cols = -tidyselect::all_of(c(group_vars,"sample_weight_variable"))) |>
      dplyr::group_by_at(c(group_vars,"name")) |>
      dplyr::summarize(estimate = stats::weighted.mean(value, w = sample_weight_variable),
                       .groups = "drop") |>
      tidyr::pivot_wider(names_from = "name", values_from = "estimate")

    estimates_y <- estimates_y |>
      dplyr::select(tidyselect::any_of(c(group_vars,"sample_weight_variable")),
                    tidyselect::starts_with("yhat"),
                    tidyselect::starts_with("effect")) |>
      dplyr::mutate(p_always_survive = p_s$always_survive) |>
      tidyr::pivot_longer(cols = -tidyselect::all_of(c(group_vars,"p_always_survive","sample_weight_variable"))) |>
      dplyr::group_by_at(c(group_vars,"name")) |>
      dplyr::summarize(estimate = stats::weighted.mean(value, w = p_always_survive * sample_weight_variable),
                       .groups = "drop") |>
      tidyr::pivot_wider(names_from = "name", values_from = "estimate")
  }

  to_return <- list(
    estimates_y = estimates_y,
    estimates_m = estimates_m,
    fit_m = fit_m,
    fit_y = fit_y,
    fit_sq_resid = fit_sq_resid,
    call = call
  )
  class(to_return) <- "pstratreg"

  return(to_return)
}
