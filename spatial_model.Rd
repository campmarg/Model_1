% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial_model.R
\name{spatial_model}
\alias{spatial_model}
\title{Spatial Model Function}
\usage{
spatial_model(Tmax, X, hab_means, HR, f, a, params)
}
\arguments{
\item{Tmax}{An integer representing the maximum time (number of time steps) for the simulation.}

\item{X}{A numeric vector representing the spatial locations - the coastline.}

\item{hab_means}{A numeric vector representing habitat means for 3 different spatial locations.}

\item{HR}{A matrix representing home range movement values.}

\item{f}{A numeric value representing fishing mortality.}

\item{a}{A numeric value representing the density-dependent recruitment parameter.}

\item{params}{A list containing model parameters created by fgparam or tparam.}

\item{MLing}{An array representing the Leslie matrix for each fishing scenario and spatial location.}

\item{CV}{a numeric representing the sampling CV of biomass}
}
\value{
A list containing the results of the spatial model simulation including N_values (settlement), Y_values (yield), and CPUE (catch per unit effort).
}
\description{
This function implements the spatial model using input the parameters and habitat means created in the other functions.
}
\author{
Margaret Campbell
}
\keyword{dynamics}
\keyword{fishing}
\keyword{model}
\keyword{mortality}
\keyword{population}
\keyword{recruitment}
\keyword{spatial}
