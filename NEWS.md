# riekelib 0.2.0.9000

## New features

* Additional covariance functions for gaussian processes:
  * `cov_rational()`
  * `cov_periodic()`
  * `cov_linear()`
  * `cov_noise()`

# riekelib 0.2.0

## New features

### Gaussian process functions

* `cholesky_decompose()`: generate the lower triangular Cholesky decomposition of 
  a matrix (consistent with Stan's implementation).
* `cov_exp_quad()`: generate a covariance matrix using the exponentiated 
  quadratic kernel.
* `condition_gaussian_process()`: generate new draws from a multivariate normal
  distribution by conditioning on a Gaussian Process.

### ggplot2 functions

* `scale_xy_percent()`, `scale_x_percent()`, and `scale_y_percent()` provide 
  shortcuts for formatting axis labels with `scales::label_percent()`.
* `scale_xy_comma()`, `scale_x_comma()`, and `scale_y_comma()` provide shortcuts
  for formatting axis labels with `scales::label_comma()`. 
  
## Minor bug fixes and improvements

* `ggquicksave()` now uses [ragg](https://ragg.r-lib.org/) by default.
* `theme_rieke()` now uses the fonts Tiempos Text and IBM Plex Sans for the 
  title and other text, respectively, by default.
* Where applicable, functions now use the `.data` pronoun from 
  [rlang](https://rlang.r-lib.org/). This removes the need for `globals.R`. 

# riekelib 0.1.0

A collection of functions I use regularly:

* Math and stats extensions
  * `logit()`, `expit()`
  * `softmax()`
  * `beta_interval()`, `normal_interval()`
* Tidyverse extensions
  * `arrange_vector()`
  * `percent()`
  * `theme_rieke()`
  * `ggquicksave()`
  * `color_text()`
* Qualtrics extensions
  * `fetch_surveys()`
  * `fix_survey_names()`
  * `deduplicate_ip()`
  * `rnps()`, `qnps()`
