% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/char2numHHI.R
\name{char2numHHI}
\alias{char2numHHI}
\title{Conversion of Hand Hardness Index (HHI)}
\usage{
char2numHHI(charHHI)
}
\arguments{
\item{charHHI}{Character string of hand hardness level, i.e., one of
\itemize{
\item Fist 'F', 4 Fingers '4F', 1 Finger '1F', Pencil 'P', Knife 'K', or Ice 'I'
\item intermediate values allowed, e.g. 'F+', '1F-', 'F-4F'
}}
}
\value{
Float value of numeric hand hardness level between 1 and 6.
}
\description{
Convert character hand hardness index (HHI) of snow layers to numeric values.
For example, hand hardness Fist becomes 1, Ice becomes 6.
}
\examples{
char2numHHI('F+')
char2numHHI('F-')
char2numHHI('F-4F')

## not meaningful:
this_throws_error <- TRUE
if (!this_throws_error) {
char2numHHI('F-P')
}

}
\author{
fherla
}
