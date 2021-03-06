% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arrays.R
\name{logspace}
\alias{logspace}
\title{Returns numbers evenly spaced in logspace.}
\usage{
logspace(start, stop, num = 15, base = 10)
}
\arguments{
\item{start}{\code{base}^\code{start}is the starting value of the sequence.}

\item{stop}{\code{base}^\code{stop} is the final value in the sequence.}

\item{num}{Number of sequence elements to generate. Default is 15.}

\item{base}{The base of the log space. Default is 10.}
}
\value{
Sequence from \code{base}^\code{start} to \code{base}^\code{stop}
evenly logspaced.
}
\description{
In linear space the sequence starts at \code{base}^\code{start} and ends at
\code{base}^\code{stop}.
}
\examples{
logspace(-5, 5)

}
