
# riekelib

**Author:** [Mark Rieke](https://www.thedatadiary.net/about/)
**License:**
[MIT](https://github.com/markjrieke/riekelib/blob/master/LICENSE.md)

This is a collection of functions I use regularly both in professional &
personal projects. Formalizing in a development package helps speed up
workflows without having to re-define functions in each project. If it
makes sense, I may coalesce common functions into their own dedicated
package, but this can be considered an ever-in-progress testing ground.

Others should note that this is intended primarily for use by me, and it
may not be the most well-documented or error-resilient.

## Installation

If you’d like to use, this package can be installed with the
[devtools](https://www.r-project.org/nosvn/pandoc/devtools.html)
package:

``` r
install.packages("devtools")
devtools::install_github("https://github.com/markjrieke/riekelib")
```

## Usage

There are currently four function groups in this package:

1.  `stats_extensions.R` contains functions that provide an interface to
    the `stats` package; namely, adding confidence intervals to common
    distributions.
2.  `bound_transforms.R` contains functions that rescale values on a
    bound scale to an unbounded scale, and vice-versa.
3.  `qualtrics_extensions.R` contains functions that build upon the
    `qualtRics` package, but would be unlikely to be accepted in a PR.
4.  `tidyverse_extensions.R` contains functions that aggregate common
    data transformation steps into single function calls.
5.  `workboots.R` contains functions that generate bootstrap prediction 
    intervals from tidymodel workflows.
