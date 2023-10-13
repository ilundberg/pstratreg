---
output: html_document
---



# Basic package functionality


```r
library(tidyverse)
library(pstratreg)
```

This page illustrates the basic functionality of the `pstratreg` function. This function conducts parametric principal stratification analysis to estimate average causal effect among the always-valid subgroup whose outcome would exist in either treatment condition.

> Too much jargon? Start with the first page on [the goal](the-goal)!

The package automates the process to

- estimate a mediator model
- estimate an outcome model
     - allowing heteroskedasticity if needed
- calculate the conditional probability of being always-valid
- implement monotonicity assumptions
- bound estimates using the conditional outcome distribution and the proportion in the always-valid subgroup
- return estimates, conditional on population subgroups if requested

## Simulate data

We first simulate some data for illustration.

The data has four variables

- continuous confounder `x`
- binary treatment `a`
- binary mediator `m`
- continuous outcome `y`
    - `y` is `NA` when `m = FALSE`


```r
data <- pstratreg_sim(n = 100)
```


```
#>            x     a     m         y
#> 1  0.1502074 FALSE  TRUE  1.918815
#> 2  0.3053517  TRUE  TRUE  2.619947
#> 3  0.7280813  TRUE  TRUE  2.064818
#> 4 -1.4277885 FALSE FALSE        NA
#> 5 -2.1012168 FALSE  TRUE -2.332822
```
    
## The `pstratreg` function

The call below runs a principal stratification regression analysis with default options, returning estimates of the average treatment effect among the latent stratum who would have valid outcomes regardless of treatment.


```r
result <- pstratreg(
  formula_y = formula(y ~ x*a),
  formula_m = formula(m ~ x*a),
  data = data,
  treatment_name = "a"
)
```


```
#> Effect on mediator, where mediator indicates whether outcome will be valid
#> # A tibble: 1 × 3
#>   mhat1 mhat0 effect_m
#>   <dbl> <dbl>    <dbl>
#> 1 0.760 0.752  0.00749
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1          0.664           1.19           1.71
```

## Positive monotonicity

If you believe that the `TRUE` value of the treatment never causes the outcome to be undefined, then you might add the `monotonicity_positive = TRUE` option.

Sometimes, the monotonicity assumption disagrees with the empirical estimates in at least some cases. For example, we assume that treatment never prevents a valid outcome but empirically we estimate that the treatment increases the probability that the treatment increases the value of the mediator in some subgroups. When this happens, the package issues a warning.

Empirical monotonicity violations may be non-troubling; they can occur in estimates due to random chance from sampling variability. Because the user has assumed monotonicity, the package assumes that any violations arise purely from estimation errors. The predicted values of the mediator under each treatment condition in these cases are forced to be equal, at the midpoint of the two predicted values.

Generally, if the warning tells you that monotonicity is violated in only a small percent of cases, it may be warranted to proceed. If monotonicity is empirically violated in many cases, then you may need to rethink this assumption.


```r
result <- pstratreg(
  formula_y = formula(y ~ x*a),
  formula_m = formula(m ~ x*a),
  data = data,
  treatment_name = "a",
  monotonicity_positive = TRUE
)
#> Warning in pstratreg(formula_y = formula(y ~ x * a), formula_m = formula(m ~ : Monotonicity violated in 56 % of cases
#> Forcing mhat1_trunc = mhat0_trunc at midpoint of estimates for those
```

```
#> Effect on mediator, where mediator indicates whether outcome will be valid
#> # A tibble: 1 × 3
#>   mhat1 mhat0 effect_m
#>   <dbl> <dbl>    <dbl>
#> 1 0.760 0.752  0.00749
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1           1.11           1.14           1.17
```

## Negative monotonicity

Conversely, you can assume negative monotonicity with `monotonicity_negative = TRUE`. If you are not sure what negative monotonicity is, see the previous page on the big idea!

In this particular simulation, negative monotonicity does not hold and you see below that the warning appropriately alerts us that monotonicity is frequently empirically violated.


```r
result <- pstratreg(
  formula_y = formula(y ~ x*a),
  formula_m = formula(m ~ x*a),
  data = data,
  treatment_name = "a",
  monotonicity_negative = TRUE
)
#> Warning in pstratreg(formula_y = formula(y ~ x * a), formula_m = formula(m ~ : Monotonicity violated in 44 % of cases
#> Forcing mhat1_trunc = mhat0_trunc at midpoint of estimates for those
#> Warning in pstratreg(formula_y = formula(y ~ x * a),
#> formula_m = formula(m ~ : Recoding 45 % of p_s to sum to 1
```

```
#> Effect on mediator, where mediator indicates whether outcome will be valid
#> # A tibble: 1 × 3
#>   mhat1 mhat0 effect_m
#>   <dbl> <dbl>    <dbl>
#> 1 0.760 0.752  0.00749
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1           1.11           1.13           1.15
```

## Aggregate in subgroups

Instead of sample average effect estimates, you might want the estimate within subgroups defined by a grouping variable in the data. The `group_vars` argument allows you to specify a vector of variable names from `data` within which to aggregate results.

First we create some groups for illustration

```r
data_with_groups <- data %>%
  mutate(group1 = x < -.5,
         group2 = x > .5)
```

```
#>            x     a     m         y group1 group2
#> 1  0.1502074 FALSE  TRUE  1.918815  FALSE  FALSE
#> 2  0.3053517  TRUE  TRUE  2.619947  FALSE  FALSE
#> 3  0.7280813  TRUE  TRUE  2.064818  FALSE   TRUE
#> 4 -1.4277885 FALSE FALSE        NA   TRUE  FALSE
#> 5 -2.1012168 FALSE  TRUE -2.332822   TRUE  FALSE
#> 6 -1.5317301 FALSE  TRUE -2.400913   TRUE  FALSE
```

and then we apply the function to estimate within those groups.

```r
result <- pstratreg(
  formula_y = formula(y ~ x*a),
  formula_m = formula(m ~ x*a),
  data = data_with_groups,
  treatment_name = "a",
  group_vars = c("group1","group2")
)
```

```
#> Effect on mediator, where mediator indicates whether outcome will be valid
#> # A tibble: 3 × 5
#>   group1 group2 mhat1 mhat0 effect_m
#>   <lgl>  <lgl>  <dbl> <dbl>    <dbl>
#> 1 FALSE  FALSE  0.835 0.845 -0.00947
#> 2 FALSE  TRUE   0.960 0.970 -0.00992
#> 3 TRUE   FALSE  0.556 0.524  0.0322 
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 3 × 5
#>   group1 group2 effect_y_lower effect_y_naive effect_y_upper
#>   <lgl>  <lgl>           <dbl>          <dbl>          <dbl>
#> 1 FALSE  FALSE           0.519          1.08            1.64
#> 2 FALSE  TRUE            1.26           1.41            1.57
#> 3 TRUE   FALSE          -0.438          0.847           2.13
```

## Sample weights

If you have case weights from sampling with unequal probabilities, they can be provided in a vector of length `nrow(data)` using the `weights` argument.

Here we generate some hypothetical weights

```r
sim_weights <- runif(nrow(data))
```

and then call the function. Note that the `glm()` used internally to estimate logistic regression will create a warning for `non-integer #successes in a binomial glm!` which is to be expected when weights are used in this function.


```r
result <- pstratreg(
  formula_y = formula(y ~ x*a),
  formula_m = formula(m ~ x*a),
  data = data,
  treatment_name = "a",
  weights = sim_weights
)
#> Warning in eval(family$initialize): non-integer #successes
#> in a binomial glm!
```

```
#> Effect on mediator, where mediator indicates whether outcome will be valid
#> # A tibble: 1 × 3
#>   mhat1 mhat0 effect_m
#>   <dbl> <dbl>    <dbl>
#> 1 0.684 0.744  -0.0602
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1          0.529           1.14           1.75
```



## Heteroskedasticity

If you believe that the conditional variance of the outcome differs as a function of covariates, you can relax the assumption of homskedasticity and estimate by a variance function regression in which the log conditional variance is modeled as in `formula_y`.

To allow heteroskedasticity, add the `homoskedastic = FALSE` option.


```r
result <- pstratreg(
  formula_y = formula(y ~ x*a),
  formula_m = formula(m ~ x*a),
  data = data,
  treatment_name = "a",
  homoskedastic = FALSE
)
```


```
#> Effect on mediator, where mediator indicates whether outcome will be valid
#> # A tibble: 1 × 3
#>   mhat1 mhat0 effect_m
#>   <dbl> <dbl>    <dbl>
#> 1 0.760 0.752  0.00749
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1          0.883           1.19           1.49
```

## Standard errors and confidence intervals

Appropriate standard errors depend on application-specific knowledge of how the data were generated. In the case of simple random samples, the package will produce

- standard errors by the nonparametric bootstrap
- 95\% confidence intervals by a normal approximation using the bootstrap standard error


```r
result_with_se <- pstratreg_se(result)
```


```
#> # A tibble: 6 × 5
#>   estimand       estimate     se ci.min ci.max
#>   <chr>             <dbl>  <dbl>  <dbl>  <dbl>
#> 1 effect_m        0.00749 0.0980 -0.185  0.200
#> 2 mhat0           0.752   0.0478  0.658  0.846
#> 3 mhat0_trunc     0.752   0.0478  0.658  0.846
#> 4 mhat1           0.760   0.0916  0.580  0.939
#> 5 mhat1_trunc     0.760   0.0916  0.580  0.939
#> 6 effect_y_lower  0.883   0.292   0.311  1.45
```
