---
output: html_document
---



# Basic functionality

This page illustrates the basic functionality of the `pstratreg` function. This function conducts parametric principal stratification analysis to estimate average causal effect among the always-valid subgroup whose outcome would exist in either treatment condition.

> Jargon? Start with the first page on [the goal]!

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
library(tidyverse)
library(pstratreg)
data <- pstratreg_sim(n = 100)
```


```
#>            x     a     m          y
#> 1 -1.1727663 FALSE FALSE         NA
#> 2  0.4261182 FALSE  TRUE  0.0289127
#> 3 -1.3707455 FALSE  TRUE -2.6234567
#> 4  0.1782202 FALSE  TRUE  2.2496029
#> 5 -0.3285690 FALSE  TRUE  1.4954604
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
#> 1 0.768 0.766  0.00196
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1          0.117          0.723           1.33
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
#> Warning in pstratreg(formula_y = formula(y ~ x * a), formula_m = formula(m ~ : Monotonicity violated in 64 % of cases
#> Forcing mhat1_trunc = mhat0_trunc at midpoint of estimates for those
```

```
#> Effect on mediator, where mediator indicates whether outcome will be valid
#> # A tibble: 1 × 3
#>   mhat1 mhat0 effect_m
#>   <dbl> <dbl>    <dbl>
#> 1 0.768 0.766  0.00196
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1          0.715          0.751          0.787
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
#> Warning in pstratreg(formula_y = formula(y ~ x * a), formula_m = formula(m ~ : Monotonicity violated in 36 % of cases
#> Forcing mhat1_trunc = mhat0_trunc at midpoint of estimates for those
```

```
#> Effect on mediator, where mediator indicates whether outcome will be valid
#> # A tibble: 1 × 3
#>   mhat1 mhat0 effect_m
#>   <dbl> <dbl>    <dbl>
#> 1 0.768 0.766  0.00196
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1          0.716          0.755          0.803
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
#>            x     a     m           y group1 group2
#> 1 -1.1727663 FALSE FALSE          NA   TRUE  FALSE
#> 2  0.4261182 FALSE  TRUE  0.02891270  FALSE  FALSE
#> 3 -1.3707455 FALSE  TRUE -2.62345667   TRUE  FALSE
#> 4  0.1782202 FALSE  TRUE  2.24960291  FALSE  FALSE
#> 5 -0.3285690 FALSE  TRUE  1.49546044  FALSE  FALSE
#> 6 -0.8268437 FALSE  TRUE  0.04852802   TRUE  FALSE
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
#> 1 FALSE  FALSE  0.820 0.839  -0.0195
#> 2 FALSE  TRUE   0.924 0.949  -0.0249
#> 3 TRUE   FALSE  0.602 0.556   0.0456
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 3 × 5
#>   group1 group2 effect_y_lower effect_y_naive effect_y_upper
#>   <lgl>  <lgl>           <dbl>          <dbl>          <dbl>
#> 1 FALSE  FALSE           0.159          0.758          1.36 
#> 2 FALSE  TRUE            0.273          0.534          0.796
#> 3 TRUE   FALSE          -0.279          0.973          2.23
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
#> 1 0.767 0.824  -0.0571
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1          0.178          0.710           1.24
```
