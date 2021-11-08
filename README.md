README
================

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

There are currently three main functions in this package:

1.  `beta_interval()` adds columns for the lower and upper bound of a
    confidence interval based on the beta distribution.
2.  `logit()` transforms a value on a bound scale to an unbounded scale
    (e.x., percentages bound by `[0, 1]` -&gt; `[-Inf, Inf]`).
3.  `expit()` transforms a value on an unbounded scale to a bound scale
    (e.x., `[-Inf, Inf]` -&gt; percentages bound by `[0, 1]`).
