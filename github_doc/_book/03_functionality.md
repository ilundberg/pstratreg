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
#>            x     a     m        y
#> 1  0.6226574  TRUE  TRUE 1.241466
#> 2  1.2984864  TRUE  TRUE 3.317720
#> 3 -0.1933131 FALSE  TRUE 1.367128
#> 4  0.6308143  TRUE  TRUE 2.207222
#> 5 -1.4314411 FALSE FALSE       NA
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
#> 1 0.907 0.764    0.143
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1          0.584          0.994           1.40
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
```

```
#> Effect on mediator, where mediator indicates whether outcome will be valid
#> # A tibble: 1 × 3
#>   mhat1 mhat0 effect_m
#>   <dbl> <dbl>    <dbl>
#> 1 0.907 0.764    0.143
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1          0.815          0.997           1.21
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
#> Warning in pstratreg(formula_y = formula(y ~ x * a), formula_m = formula(m ~ : Monotonicity violated in 100 % of cases
#> Forcing mhat1_trunc = mhat0_trunc at midpoint of estimates for those
```

```
#> Effect on mediator, where mediator indicates whether outcome will be valid
#> # A tibble: 1 × 3
#>   mhat1 mhat0 effect_m
#>   <dbl> <dbl>    <dbl>
#> 1 0.907 0.764    0.143
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1           1.00           1.00           1.00
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
#>            x     a     m        y group1 group2
#> 1  0.6226574  TRUE  TRUE 1.241466  FALSE   TRUE
#> 2  1.2984864  TRUE  TRUE 3.317720  FALSE   TRUE
#> 3 -0.1933131 FALSE  TRUE 1.367128  FALSE  FALSE
#> 4  0.6308143  TRUE  TRUE 2.207222  FALSE   TRUE
#> 5 -1.4314411 FALSE FALSE       NA   TRUE  FALSE
#> 6  1.5281387  TRUE  TRUE 3.781825  FALSE   TRUE
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
#> 1 FALSE  FALSE  0.916 0.799   0.117 
#> 2 FALSE  TRUE   0.955 0.923   0.0316
#> 3 TRUE   FALSE  0.841 0.542   0.299 
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 3 × 5
#>   group1 group2 effect_y_lower effect_y_naive effect_y_upper
#>   <lgl>  <lgl>           <dbl>          <dbl>          <dbl>
#> 1 FALSE  FALSE           0.589          1.01            1.44
#> 2 FALSE  TRUE            0.715          0.924           1.13
#> 3 TRUE   FALSE           0.302          1.10            1.90
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
#> 1 0.912 0.767    0.146
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1          0.687           1.08           1.47
```
