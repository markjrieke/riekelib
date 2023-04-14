# riekelib (development version)

* Change default backend for plot rendering to [ragg](https://ragg.r-lib.org/). 
* Add functions for working with Gaussian processes:
  * `cholesky_decompose()`: generate the lower triangular Cholesky decomposition of a matrix.
  * `cov_exp_quad()`: generate a covariance matrix using the exponentiated quadratic kernel. 
  * `condition_gaussian_process()`: generate new draws from a multivariate normal distribution by conditioning on a Gaussian Process.
* Add shortcut functions for formatting scales
  * `scale_xy_percent()`, `scale_x_percent()`, `scale_y_percent()`
  * `scale_xy_comma()`, `scale_x_comma()`, `scale_y_comma()`
* Update `theme_rieke()` to use Tiempos Text/IBM Plex Sans for the title/text by default

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
