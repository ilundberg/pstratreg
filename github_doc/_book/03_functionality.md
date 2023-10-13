---
output: html_document
---



# Package functionality

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
#>            x     a    m          y
#> 1 -0.7538333 FALSE TRUE -0.0399594
#> 2  0.8723765 FALSE TRUE  2.0095221
#> 3 -0.2098973 FALSE TRUE -0.7560401
#> 4  0.2649635  TRUE TRUE  0.8823682
#> 5  0.8949594 FALSE TRUE -0.2285640
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
#> 1 0.630 0.668  -0.0387
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1         -0.372          0.276          0.924
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
#> Warning in pstratreg(formula_y = formula(y ~ x * a), formula_m = formula(m ~ : Monotonicity violated in 55 % of cases
#> Forcing mhat1_trunc = mhat0_trunc at midpoint of estimates for those
```

```
#> Effect on mediator, where mediator indicates whether outcome will be valid
#> # A tibble: 1 × 3
#>   mhat1 mhat0 effect_m
#>   <dbl> <dbl>    <dbl>
#> 1 0.630 0.668  -0.0387
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1          0.235          0.297          0.367
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
#> Warning in pstratreg(formula_y = formula(y ~ x * a), formula_m = formula(m ~ : Monotonicity violated in 45 % of cases
#> Forcing mhat1_trunc = mhat0_trunc at midpoint of estimates for those
#> Warning in pstratreg(formula_y = formula(y ~ x * a),
#> formula_m = formula(m ~ : Recoding 52 % of p_s to sum to 1
```

```
#> Effect on mediator, where mediator indicates whether outcome will be valid
#> # A tibble: 1 × 3
#>   mhat1 mhat0 effect_m
#>   <dbl> <dbl>    <dbl>
#> 1 0.630 0.668  -0.0387
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1          0.210          0.294          0.374
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
#>            x     a    m          y group1 group2
#> 1 -0.7538333 FALSE TRUE -0.0399594   TRUE  FALSE
#> 2  0.8723765 FALSE TRUE  2.0095221  FALSE   TRUE
#> 3 -0.2098973 FALSE TRUE -0.7560401  FALSE  FALSE
#> 4  0.2649635  TRUE TRUE  0.8823682  FALSE  FALSE
#> 5  0.8949594 FALSE TRUE -0.2285640  FALSE   TRUE
#> 6  0.1491256 FALSE TRUE  0.6851299  FALSE  FALSE
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
#> 1 FALSE  FALSE  0.764 0.737   0.0266
#> 2 FALSE  TRUE   0.942 0.869   0.0728
#> 3 TRUE   FALSE  0.360 0.510  -0.151 
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 3 × 5
#>   group1 group2 effect_y_lower effect_y_naive effect_y_upper
#>   <lgl>  <lgl>           <dbl>          <dbl>          <dbl>
#> 1 FALSE  FALSE         -0.364           0.293          0.950
#> 2 FALSE  TRUE          -0.0873          0.200          0.486
#> 3 TRUE   FALSE         -0.904           0.377          1.66
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
#> 1 0.619 0.714  -0.0956
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1         -0.536         0.0321          0.601
```
