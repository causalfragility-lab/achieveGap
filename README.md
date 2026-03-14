# achieveGap <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/causalfragility-lab/achieveGap/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/causalfragility-lab/achieveGap/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/achieveGap)](https://CRAN.R-project.org/package=achieveGap)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

## Overview

**achieveGap** provides a joint hierarchical penalized spline framework for
estimating achievement gap *trajectories* in longitudinal educational data.

Most existing approaches model group-specific trajectories separately and
derive the gap as a post hoc difference — ignoring covariance between estimates
and producing incorrect uncertainty quantification. **achieveGap** parameterizes
the gap directly as a smooth function of grade, estimated simultaneously with
the baseline trajectory within a mixed-effects model. Simultaneous confidence
bands with correct joint coverage are constructed via posterior simulation.

## Key Features

- **Direct gap estimation**: the gap function is a primary model parameter,
  not a derived quantity
- **Simultaneous confidence bands**: correct joint coverage over the grade domain
  via posterior simulation (Marra & Wood, 2012)
- **REML smoothing**: automatic, data-adaptive smoothness selection
- **Hierarchical structure**: school- and student-level random effects
- **Hypothesis testing**: global and grade-interval tests with multiplicity
  control
- **Simulation tools**: built-in data generation and benchmark simulation study

## Installation
```r
# CRAN (once published)
install.packages("achieveGap")

# Development version from GitHub
# install.packages("devtools")
devtools::install_github("causalfragility-lab/achieveGap")
```

## Quick Example
```r
library(achieveGap)

# 1. Simulate data
sim <- simulate_gap(n_students = 400, n_schools = 30,
                    gap_shape = "monotone", seed = 2024)

# 2. Fit the model
fit <- gap_trajectory(
  data    = sim$data,
  score   = "score",
  grade   = "grade",
  group   = "SES_group",
  school  = "school",
  student = "student"
)

# 3. Summarize and visualize
summary(fit)
plot(fit, grade_labels = c("K","G1","G2","G3","G4","G5","G6","G7"))

# 4. Test the gap
test_gap(fit, type = "both")
```

## Model

The model takes the form:

$$Y_{ijt} = \beta_0 + f_0(t) + G_{ij} f_1(t) + Z_{ijt}^\top \delta + u_j + v_i + \epsilon_{ijt}$$

where:
- $f_0(t)$ is the baseline smooth trajectory (reference group)
- $f_1(t)$ is the gap trajectory — the primary estimand
- $G_{ij}$ is a binary group indicator (0 = reference, 1 = focal)
- $u_j$, $v_i$ are school- and student-level random intercepts

Both smooth functions are penalized cubic regression splines estimated via
`mgcv::gamm()` with REML.

## Functions

| Function | Description |
|---|---|
| `gap_trajectory()` | Fit the joint hierarchical spline model |
| `plot()` | Gap trajectory with simultaneous/pointwise bands |
| `summary()` | Table of gap estimates and significance |
| `test_gap()` | Global and simultaneous hypothesis tests |
| `fit_separate()` | Comparison: separate splines per group |
| `simulate_gap()` | Generate synthetic longitudinal data |
| `run_simulation()` | Benchmark simulation study (reproduces paper tables) |
| `summarize_simulation()` | Print Tables 1 & 2 from simulation results |

## Paper

This package accompanies:

> Hait, S. (2024). Modeling Achievement Gap Trajectories Using Hierarchical
> Penalized Splines: A Mixed Effects Framework with an R Implementation.
> *Journal of Educational and Behavioral Statistics*.

## References

- Eilers, P. H. C., & Marx, B. D. (1996). Flexible smoothing with B-splines
  and penalties. *Statistical Science*, 11(2), 89–121.
  <https://doi.org/10.1214/ss/1177012843>
- Marra, G., & Wood, S. N. (2012). Coverage properties of confidence intervals
  for generalized additive model components. *Scandinavian Journal of
  Statistics*, 39(1), 53–74.
  <https://doi.org/10.1111/j.1467-9469.2011.00760.x>
- Wood, S. N. (2017). *Generalized Additive Models: An Introduction with R*
  (2nd ed.). CRC Press.
  <https://doi.org/10.1201/9781315370279>

## License

GPL (>= 3)
