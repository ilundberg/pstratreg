---
output: html_document
---



# Relaxing homoskedasticity

Model-based principal stratification bounds involve a model for the conditional distribution of the outcome, not just the conditional mean. For that goal, one might be concerned about the homoskedasticity assumption.

> Homoskedasticity: The assumption that the conditional outcome variance is equal at all values of the predictors.

While commonly assumed in Ordinary Least Squares, homoskedasticity is an assumption that can be relaxed. We currently support parametric model for heteroskedasticity based on the ideas of variance function regression ([Western \& Bloome 2009](https://doi.org/10.1111/j.1467-9531.2009.01222.x)).

## Model the conditional mean

We begin with an ordinary least squares outcome model, as in the homoskedastic case,

$$\begin{aligned}
\hat{E}(Y\mid \vec{X},A,M = 1) = \hat\alpha + A\hat\beta  + \vec{X}'\hat{\vec\gamma} + A\vec{X}'\hat{\vec\eta}
\end{aligned}$$

where $A$ is a binary treatment, $\vec{X}$ is a vector of measured confounders, and $M = 1$ is the mediator indicating that the outcome is valid. Let $\hat{Y}_i$ be the predicted value for each unit $i$ from this model.

## Model the conditional variance

For each unit, we then calculate the squared residual, which we can conceptualize as the unit-specific contribution to the log conditional variance.

$$\hat\epsilon_i^2 = \left(Y_i - \hat{Y}_i\right)^2$$

To relax homoskedasticity, we estimate a second model for the log squared residuals, where the log is used to ensure that these are on an unbounded scale.

$$\begin{aligned}
\hat{E}(\log(\hat\epsilon_i^2)\mid \vec{X},A,M = 1) = \hat\lambda + A\hat\delta  + \vec{X}'\hat{\vec\nu} + A\vec{X}'\hat{\vec\eta}
\end{aligned}$$

Predictions from this model (exponentiated) are estimates of the conditional variance, $\hat{V}(Y\mid \vec{X},A,M=1)$ at the observed predictors for each unit. We make predictions under the treatment and control conditions.

$$\begin{aligned}
\hat{V}(Y\mid \vec{X},A = 1,M=1) &= \text{exp}\left[\hat\lambda + \hat\delta  + \vec{X}'\left(\hat{\vec\nu} + \hat{\vec\eta}\right)\right] \\
\hat{V}(Y\mid \vec{X},A = 0,M=1) &= \text{exp}\left[\hat\lambda + \vec{X}'\hat{\vec\nu}\right] \\
\end{aligned}$$

## In code

You can relax the homskedasticity assumption with the `homoskedastic = FALSE` argument. Doing so estimates a variance function regression (as above) using the same predictor model formula as in `formula_y`.


```r
library(tidyverse)
library(pstratreg)
data <- pstratreg_sim(n = 100)
```

With the data setup above, we can estimate the models.


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
#> 1 0.835 0.751   0.0844
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1           1.01           1.27           1.54
```
