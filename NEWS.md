# riekelib (development version)

* Change default backend for plog rendering to [ragg](https://ragg.r-lib.org/). 
* Add functions for working with Gaussian processes:
  * `cholesky_decompose()`: generate the lower triangular Cholesky decomposition of a matrix.
  * `cov_exp_quad()`: generate a covariance matrix using the exponentiated quadratic kernel. 

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
