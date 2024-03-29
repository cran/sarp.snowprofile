% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-SPpairs.R
\docType{data}
\name{SPpairs}
\alias{SPpairs}
\title{Pairs of example snowprofiles}
\format{
A list with several entries, that are of class \link{snowprofile}
}
\usage{
SPpairs
}
\description{
A list with several entries, each containing a snowprofile object. Pairs of similar profiles are grouped by their names.
}
\examples{
## Each name refers to one snowprofile:
names(SPpairs)

opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))
plot(SPpairs$A_manual, main = 'SPpairs$A_manual')
plot(SPpairs$A_modeled, main = 'SPpairs$A_modeled')
par(opar)

}
\seealso{
\link{SPgroup}, \link{SPtimeline}
}
\keyword{object}
\keyword{snowprofile}
