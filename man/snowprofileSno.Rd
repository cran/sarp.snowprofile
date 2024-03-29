% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/snowprofileSno.R
\name{snowprofileSno}
\alias{snowprofileSno}
\title{Construct snowprofile object from SNO file}
\usage{
snowprofileSno(Filename)
}
\arguments{
\item{Filename}{path to sno file}
}
\value{
a \link{snowprofile} object
}
\description{
Read .sno files from SNOWPACK model input/output
}
\details{
Several SNOWPACK model output formats exist see \href{https://models.slf.ch/docserver/snowpack/html/snowpackio.html}{SNOWPACK documentation}

Definitions of SNO files are provided at \href{https://models.slf.ch/docserver/snowpack/html/smet.html}{https://models.slf.ch/docserver/snowpack/html/smet.html}
}
\examples{

## Path to example prf file
Filename <- system.file('extdata', 'example.sno', package = 'sarp.snowprofile')

## Read snowprofile object
Profile <- snowprofileSno(Filename)

## Note: plot.snowprofile won't work because sno files don't have harndess

## Plot a temperautre profile
plot(snowprofileSet(list(Profile)), ColParam = 'temp')

}
\seealso{
\link{snowprofilePro}, \link{snowprofilePrf}, \link{snowprofileCsv}
}
\author{
shorton
}
