% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simpleROC.R
\name{SimpleROC}
\alias{SimpleROC}
\title{SimpleROC function}
\usage{
SimpleROC(labels, scores)
}
\arguments{
\item{labels}{ground truth boolean vector}

\item{scores}{numeric vector with probability of a given classification according to the classifier}
}
\description{
This function builds a ROC for a classifier
it returns a data frame with TPR and FPR from which to plot the ROC
}
\examples{
SimpleROC(sample(c(T,F), 1000, replace = T), runif(1000)) #should return an identity line ROC
}
\keyword{ROC}
