
# pstratreg

Regression-Based Principal Stratification

**WARNING.** This website documents an R package that is in development. Expect changes. We encourage you to try the package and email us with suggestions. Project is joint work by Ian Lundberg ([ilundberg@cornell.edu](mailto:ilundberg@cornell.edu)) and Soonhong Cho ([soonhongcho@g.ucla.edu](mailto:soonhongcho@g.ucla.edu)).

This website documents the **pstratreg** package for R. The package helps study causal effects when some treatment values cause outcomes to be non-existent for some units.

Possible applications include

- a labor market intervention where the outcome is hourly wage, but some people are unemployed
- a medical intervention where the outcome is a health metric, but some people die before it is measured
- a sociological study where the outcome involves one's spouse, but some people divorce or never marry and thus have no spouse

This package provides regression-based methods for principal stratification that rely on parametric models and are useful in settings where one hopes to adjust for many confounders.

## Getting started

To get started, first install [R and RStudio](https://rstudio-education.github.io/hopr/starting.html). Then install the package.

```{r, eval = F}
devtools::install_github("ilundberg/pstratreg")
```
  
Now head on to the next page to see the types of questions this package can help answer.

This project is joint work between Ian Lundberg (Cornell, [ilundberg@cornell.edu](mailto:ilundberg@cornell.edu)) and Soonhong Cho (UCLA, [soonhongcho@g.ucla.edu](mailto:soonhongcho@g.ucla.edu)).