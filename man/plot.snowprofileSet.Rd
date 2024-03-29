% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.snowprofileSet.R
\name{plot.snowprofileSet}
\alias{plot.snowprofileSet}
\title{Plot a single layer property in multiple profiles side-by-side}
\usage{
\method{plot}{snowprofileSet}(
  x,
  SortMethod = c("time", "unsorted", "hs", "elev", "presorted"),
  ColParam = c("gtype", "hardness", "density", "temp", "gsize", "ssi", "p_unstable",
    "crit_cut_length", "rta", "percentage"),
  TopDown = FALSE,
  DateStart = NA,
  DateEnd = NA,
  Timeseries_labels = c("weekly", "monthly", NA),
  ylim = NULL,
  OutlineLyrs = FALSE,
  emphasizeLayers = NULL,
  colAlpha = NA,
  colEmphasis = NA,
  OutlineProfile = NULL,
  HorizGrid = TRUE,
  VerticalGrid = TRUE,
  yaxis = TRUE,
  main = NA,
  ylab = NA,
  xlab = NA,
  box = TRUE,
  xticklabels = FALSE,
  xtick.las = 2,
  yPadding = 10,
  xPadding = 0.5,
  hardnessResidual = 1,
  hardnessScale = 1,
  hardnessOffset = -0.5,
  k = NULL,
  offset = as.Date(NA),
  add = FALSE,
  ...
)
}
\arguments{
\item{x}{An object of class \link{snowprofileSet}}

\item{SortMethod}{How to arrange profiles along the x-axis. Options include timeseries (default = 'time'), in existing order of Profiles list ('unsorted'), sorted by HS ('hs'), or elevation ('elev')}

\item{ColParam}{What parameter to show with colour. So far the following types are available: "gtype", "hardness", "density", "temp", "gsize", "ssi", "p_unstable", "crit_cut_length", "rta", "percentage".}

\item{TopDown}{Option to plot by depth instead of height with zero depth on top of plot (default = FALSE)}

\item{DateStart}{Start date for timeseries plots (\code{SortMethod = 'time'}). If not provided, the function takes the date range from Profiles (default = NA).}

\item{DateEnd}{End date for timeseries plots (\code{SortMethod = 'time'}). If not provided, the function takes the date range from Profiles (default = NA).}

\item{Timeseries_labels}{Label Saturdays "weekly", "monthly", or \code{NA}}

\item{ylim}{Vertical range of plot}

\item{OutlineLyrs}{Switch for outlining layers (default = FALSE)}

\item{emphasizeLayers}{emphasize layers with different transparency than others, or a different color altogether? then set this argument to \code{TRUE} if you want to emphasize all
labeled layers of interest (aka weak layers), or provide a named list with arguments to a function call to \link{findPWL} to define which layers to emphasize.
Set either \code{colAlpha} or \code{colEmphasis} to make the emphasis apparent.}

\item{colAlpha}{the transparency setting for all layers (except the ones to be emphasized if you want to emphasize any). This can be useful for example
if you want to overplot the grain type sequences with another variable, e.g. a percentage from a distribution.}

\item{colEmphasis}{the color of the layers to be emphasized (only if you want a different color than defined by \code{ColParam})}

\item{OutlineProfile}{vector of profile indices that will be outlined to highlight them}

\item{HorizGrid}{Draw horizontal grid at layer heights (default = TRUE)}

\item{VerticalGrid}{Draw vertical grid at xticks (default = TRUE)}

\item{yaxis}{draw a y-axis? (either \code{FALSE}, \code{TRUE} draws yaxis left, \code{"right"} draws yaxis on the right plot side)
\emph{Note} that in case of \code{"right"} you need to adjust \code{par(mar = ...)}, disable \code{ylab} and manually draw an xlab with \code{mtext}.}

\item{main}{Main title}

\item{ylab}{y-axis label; disable ylab by providing an empty string (i.e., ylab = '')}

\item{xlab}{x-axis label; disable xlab by providing an empty string (i.e., xlab = '')}

\item{box}{Draw a box around the plot (default = TRUE)}

\item{xticklabels}{Label the profiles with their "names", "originalIndices" (prior to sorting), "dates", or a custom character array}

\item{xtick.las}{Orientation of labels if xticklabels is specified.}

\item{yPadding}{Padding between ylim and limits of data, default = 10.
Note that R will still put padding by default. If you want to prohibit that entirely, specify \code{xaxs ='i'}, or \code{yaxs = 'i'}.}

\item{xPadding}{Padding between xlim and limits of data, default = 0.5.
Note that R will still put padding by default. If you want to prohibit that entirely, specify \code{xaxs = 'i'}, or \code{yaxs = 'i'}. For xPadding, you can provide either a scalar, or
a length 2 numeric for left and right hand side, respectively.}

\item{hardnessResidual}{Value within \verb{[0, 1]} to control the minimum horizontal space of each layer that will be colored
irrespective of the layer's hardness. A value of \code{1} corresponds to no hardness being shown.}

\item{hardnessScale}{A scaling factor that exaggerates the hardness profile to subsequent cells on the x-axis. Useful
for time series of sparse profile observations. Note that this scaling factor is unused when \code{hardnessScale = 1} and that
it gets more influential the smaller \code{hardnessScale} gets. Also note, that a \code{hardnessScale > 1} can lead to profiles overlapping.}

\item{hardnessOffset}{offsets the profile location on the x-axis}

\item{k}{a sorting vector if \code{SortMethod = "presorted"}.}

\item{offset}{Provide a Date or POSIXct offset if you want to offset the vertical snow height/depth axis so that the offset date aligns with snow depth/height 0.}

\item{add}{add the plot to an existing plot, or create new plot?}

\item{...}{Additional parameters passed to plot()}
}
\description{
A flexible function to plot multiple snowprofiles either in a timeseries or various types of groups.
}
\details{
The routine allows you to plot coloured sequences only, or to include hardness profile information as well. See parameter
\code{hardnessResidual} and the examples for more details.
To change the font size of labels etc, use \code{par()} with the parameters \code{cex.lab}, \code{cex.axis}, etc.
}
\examples{

## Standard profile timeline (e.g. https://niviz.org)
plot(SPtimeline)

## Group of profiles with same timestamp
plot(SPgroup, SortMethod = 'unsorted')  # sorted in same order as list
plot(SPgroup, SortMethod = 'hs') # sorted by snow height
plot(SPgroup, SortMethod = 'elev') # sorted by elevation

## Colour layers by other properties
plot(SPtimeline, ColParam = 'density')

## Align layers by depth instead of height
plot(SPtimeline, TopDown = TRUE)

## Timelines with specific date ranges
plot(SPtimeline, DateEnd = '2017-12-17')
plot(SPtimeline, DateStart = '2017-12-15', DateEnd = '2017-12-17')

## Show hardness profile, too:
plot(SPtimeline, hardnessResidual = 0.5)

## Additional examples of plot dimensions and labelling
## Label the indices of the profiles in the list:
plot(SPgroup, SortMethod = 'elev', xticklabels = "originalIndices")
##  ... and with minimized axis limits and their station ID names:
plot(SPgroup, SortMethod = 'elev', xticklabels = sapply(SPgroup, function(x) x$station_id),
       yPadding = 0, xPadding = 0, xaxs = 'i', yaxs = 'i')
##  sorted by depth, and without box:
plot(SPgroup, SortMethod = 'hs', TopDown = TRUE, box = FALSE)

## Apply a date offset to investigate which layers formed around that day of interest:
pwl_exists <- sapply(SPgroup, function(sp)
  {length(findPWL(sp, pwl_date = "2019-01-21", pwl_gtype = c("SH", "DH"),
                  date_range_earlier = as.difftime(2, unit = "days"))) > 0})
k <- order(pwl_exists, decreasing = TRUE)
plot(SPgroup, SortMethod = 'presorted', k = k, xticklabels = "originalIndices",
     offset = as.Date("2019-01-21"), xlab = "<-- Jan 21 PWL exists | does not exist -->")
abline(v = max(which(pwl_exists[k]))+0.5, lty = "dashed")

## Emphasize specific layers
## (i) all labeled layers of interest:
SPgroup <- snowprofileSet(lapply(SPgroup, labelPWL))  # label layers with default settings
plot(SPgroup, SortMethod = "hs", emphasizeLayers = TRUE, colAlpha = 0.3)
## (ii) specific individual layers:
plot(SPgroup, SortMethod = "hs",
     emphasizeLayers = list(pwl_gtype = c("SH", "DH"), pwl_date = "2019-01-21"),
     colAlpha = 0.3, colEmphasis = "black")


}
\seealso{
\link{plot.snowprofile}, \link{SPgroup}
}
\author{
shorton, fherla, phaegeli
}
