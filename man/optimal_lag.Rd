% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimal_lag.R
\name{optimal_lag}
\alias{optimal_lag}
\title{Calculate Optimal Lag for Longitudinal Designs}
\usage{
optimal_lag(i, c, d, r, interval, reciprocal = TRUE)
}
\arguments{
\item{i}{numeric. Stability of independent variable.}

\item{c}{numeric. Effect of independent on dependent variable.}

\item{d}{numeric. Stability of dependent variable.}

\item{r}{numeric. Effect of dependent on independent.}

\item{interval}{numeric. Current interval (e.g., 6 months).}
}
\description{
Calculate Optimal Lag for Longitudinal Designs.
}
