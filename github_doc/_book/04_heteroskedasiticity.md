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
E(Y\mid \vec{X},A,M = 1) =\alpha + A\hat\beta  + \vec{X}\vec\gamma + A\vec{X}'\vec\eta
\end{aligned}$$

where $A$ is a binary treatment, $\vec{X}$ is a vector of measured confounders, and $M = 1$ is the mediator indicating that the outcome is valid. Let $\hat{Y}_i$ be the predicted value for each unit $i$ from this model.

## Model the conditional variance

We next allow the conditional variance of $Y$ to vary as a function of $A$ and $\vec{X}$. We first define the squared residual

$$\hat\epsilon^2 = \left(Y - \hat{Y}\right)^2$$
Under the assumption that $\hat\epsilon$ is normally distributed, the squared residual $\hat\epsilon^2$ follows a Gamma distribution with mean equal to the conditional variance $\sigma^2(\vec{X},A,M=1)$. We therefore model $\hat\epsilon^2$ by a Gamma GLM with a log link function, using a parametric linear predictor such as the one below.

$$\begin{aligned}
\log(\sigma^2(\vec{X},A,M=1)) = \lambda + A\delta  + \vec{X}'\vec\nu + A\vec{X}'\vec\omega
\end{aligned}$$

Predictions from this model (exponentiated) are estimates of the conditional variance, $\hat{V}(Y\mid \vec{X},A,M=1)$ at the observed predictors for each unit. We make predictions under the treatment and control conditions.

$$\begin{aligned}
\hat{V}(Y\mid \vec{X},A = 1,M=1) &= \text{exp}\left[\hat\lambda + \hat\delta  + \vec{X}'\left(\hat{\vec\nu} + \hat{\vec\omega}\right)\right] \\
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
#> 1 0.895 0.759    0.136
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1       -0.00557          0.516           1.04
```

Optionally, you can specify a separate model formula for the model of squared residuals that may be simpler than the model formula used for $Y$, which might be done if the model formula involves many parameters and you see errors from the internal `glm()` call about convergence for the variance regression.


```r
result <- pstratreg(
  formula_y = formula(y ~ x*a),
  formula_m = formula(m ~ x*a),
  formula_sq_resid = formula(~ x + a),
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
#> 1 0.895 0.759    0.136
#> 
#> Effect on outcome among those who would have a valid outcome regardless of treatment
#> # A tibble: 1 × 3
#>   effect_y_lower effect_y_naive effect_y_upper
#>            <dbl>          <dbl>          <dbl>
#> 1        -0.0133          0.516           1.05
```
