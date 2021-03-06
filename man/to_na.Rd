% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missing.R
\name{to_na}
\alias{to_na}
\title{Set an arbitrary number of elements in a vector to NA}
\usage{
to_na(x, ...)
}
\arguments{
\item{x}{A vector with elements that should set to NA}

\item{...}{The elements that should be set to NA}
}
\value{
Vector with specified elements set to NA
}
\description{
Will additionally set all NaN elements to NA.
}
\examples{
to_na(c(1, NaN, 2, 3), 2, NaN)

}
