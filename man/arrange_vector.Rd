% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidyverse_extensions.R
\name{arrange_vector}
\alias{arrange_vector}
\title{Sort a vector by alphabetic or numeric order}
\usage{
arrange_vector(x, desc = FALSE)
}
\arguments{
\item{x}{the vector to be sorted}

\item{desc}{logical; if set to true, will return a vector sorted in descending order}
}
\description{
Sort a vector by alphabetic or numeric order
}
\examples{
# create a character vector
myvec <- c("dog", "cat", "bird")

# arrange vector by character
arrange_vector(myvec)

# can also arrange in reverse order
arrange_vector(myvec, desc = TRUE)
}
