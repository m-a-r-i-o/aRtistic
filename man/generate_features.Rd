% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generateFeatures.R
\name{generate_features}
\alias{generate_features}
\title{generate_features function}
\usage{
generate_features(path = "./", seed = 37, diagno.on = FALSE,
  cluster.plot = FALSE, incomplete.gray = "#00334D34",
  complete.black = "#12031A", FOV.red = "#A05100", feature.plot = FALSE,
  profbins = 4, xi_FOV = 0, alpha_FOV = 0.5, beta_FOV = 0.5,
  miscentering = 0, min_visible_type = -1, max_visible_type = 5)
}
\arguments{
\item{path}{where to look for the MOCCA extended snapshot}

\item{seed}{(currently unused) rng seed}

\item{diagno.on}{TRUE to run additional diagnostics}

\item{cluster.plot}{TRUE to plot the cluster, completeness, FOV, etc}

\item{incomplete.gray}{color for plotting stars discarded due to completeness}

\item{complete.black}{color for plotting stars not discarded}

\item{FOV.red}{color to plot the FOV boudaries}

\item{feature.plot}{TRUE to plot the features}

\item{profbins}{number of bins of the surface brightness profile}

\item{xi_FOV}{FOV position along the x axis in units of rh}

\item{alpha_FOV}{FOV size along the x axis in units of rh}

\item{beta_FOV}{FOV size along the y axis in units of rh}

\item{miscentering}{amount of miscentering in units of rh}

\item{min_visible_type}{minimum star type that is considered visible, leave it set to -1 unless you know what you are doing}

\item{max_visible_type}{maximum star type that is considered visible, 3 supposedly includes only MS stars, 5 (or was it 6?) includes Abbas Askar's favourite stars, 10 was my initial choice}
}
\description{
This function generates features from a given snapshot file
}
\examples{
generate_features(...)
}
\keyword{features}
