% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makegrid.R
\name{makegrid}
\alias{makegrid}
\title{makegrid function}
\usage{
makegrid(df, k = 1, epsi = 0.1)
}
\arguments{
\item{df}{numeric dataframe}

\item{k}{fill factor}

\item{epsi}{generate random numbers in the range [(1-epsi)*min, (1+epsi)*max[}
}
\description{
This function takes a numeric dataframe and outputs a random grid of
points covering the volume spanned by the dataframe in column space
the number of rows is k that of the original dataframe
}
\examples{
makegrid(data.frame(u=rnorm(10),v=rnorm(10)))
}
\keyword{grid}
