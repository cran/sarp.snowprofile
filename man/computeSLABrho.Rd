% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/computeSLAB.R
\name{computeSLABrho}
\alias{computeSLABrho}
\title{Compute mean density of slab}
\usage{
computeSLABrho(profile)
}
\arguments{
\item{profile}{\link{snowprofile} object}
}
\value{
snowprofile object with added layers column \verb{$slab_rho}. Note that topmost layer is always \code{NA}.
}
\description{
For each layer, compute the average density of all layers above, i.e. \verb{<rho>_slab}.
}
\author{
fherla
}
