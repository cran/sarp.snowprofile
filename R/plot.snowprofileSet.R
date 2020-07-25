#' Plot a single layer property in multiple profiles side-by-side
#'
#' A flexible function to plot multiple snowprofiles either in a timeseries or various types of groups
#'
#' @param x An object of class [snowprofileSet]
#' @param SortMethod How to arrange profiles along the x-axis. Options include timeseries (default = 'time'), in existing order of Profiles list ('unsorted'), sorted by HS ('hs'), or elevation ('elev')
#' @param ColParam What parameter to show with colour. So far the following types are available: graintype (default), hardness, temperature, density, grainsize.
#' @param TopDown Option to plot by depth instead of height with zero depth on top of plot (default = FALSE)
#' @param DateStart Start date for timeseries plots (`SortMethod = 'time'`). If not provided, the function takes the date range from Profiles (default = NA).
#' @param DateEnd End date for timeseries plots (`SortMethod = 'time'`). If not provided, the function takes the date range from Profiles (default = NA).
#' @param ylim Vertical range of plot
#' @param OutlineLyrs Switch for outlining layers (default = FALSE)
#' @param HorizGrid Draw horizontal grid at layer heights (default = TRUE)
#' @param main Main title
#' @param ylab y-axis label; disable ylab by providing an empty string (i.e., ylab = '')
#' @param xlab x-axis label; disable xlab by providing an empty string (i.e., xlab = '')
#' @param box Draw a box around the plot (default = TRUE)
#' @param labelOriginalIndices Label the original (i.e. prior to sorting) indices of the profiles at the x-axis? (default = FALSE)
#' @param yPadding Padding between ylim and limits of data, default = 10.
#' Note that R will still put padding by default. If you want to prohibit that entirely, specify `xaxs ='i'`, or `yaxs = 'i'`.
#' @param xPadding Padding between xlim and limits of data, default = 0.5.
#' Note that R will still put padding by default. If you want to prohibit that entirely, specify `xaxs = 'i'`, or `yaxs = 'i'`.
#' @param ... Additional parameters passed to plot()
#'
#' @seealso [plot.snowprofile], [SPgroup]
#'
#' @author shorton, fherla, phaegeli
#'
#' @examples
#'
#' ## Standard profile timeline (e.g. https://niviz.org)
#' plot(SPtimeline)
#'
#' ## Group of profiles with same timestamp
#' plot(SPgroup, SortMethod = 'unsorted')  # sorted in same order as list
#' plot(SPgroup, SortMethod = 'hs') # sorted by snow height
#' plot(SPgroup, SortMethod = 'elev') # sorted by elevation
#'
#' ## Colour layers by other properties
#' plot(SPtimeline, ColParam = 'density')
#'
#' ## Align layers by depth instead of height
#' plot(SPtimeline, TopDown = TRUE)
#'
#' ## Timelines with specific date ranges
#' plot(SPtimeline, DateEnd = '2017-12-17')
#' plot(SPtimeline, DateStart = '2017-12-15', DateEnd = '2017-12-17')
#'
#' ## Additional examples of plot dimensions and labelling
#' ## Label the indices of the profiles in the list:
#' plot(SPgroup, SortMethod = 'elev', labelOriginalIndices = TRUE)
#' ##  ... and with minimized axis limits:
#' plot(SPgroup, SortMethod = 'elev', labelOriginalIndices = TRUE,
#'        yPadding = 0, xPadding = 0, xaxs = 'i', yaxs = 'i')
#' ##  sorted by depth, and without box:
#' plot(SPgroup, SortMethod = 'hs', TopDown = TRUE, box = FALSE)
#'
#' @export
#'
plot.snowprofileSet <- function(x,
                                SortMethod = "time",
                                ColParam = "gtype",
                                TopDown = FALSE,
                                DateStart = NA,
                                DateEnd = NA,
                                ylim = NULL,
                                OutlineLyrs = FALSE,
                                HorizGrid = TRUE,
                                main = NA,
                                ylab = NA,
                                xlab = NA,
                                box = TRUE,
                                labelOriginalIndices = FALSE,
                                yPadding = 10,
                                xPadding = 0.5,
                                ...) {


  ## ---- Check input parameters ----

  ## Profiles
  Profiles <- x
  if (!inherits(Profiles, 'snowprofileSet'))
    stop("plot.snowprofileSet requires an object of class snowprofileSet")

  ## SortMethod
  if (!(SortMethod %in% c("time", "hs", "elev", "unsorted")))
    stop(paste("SortMethod", SortMethod, "not supported ('time', 'hs', 'elev', 'unsorted')"))

  ## ColParam
  if (!(ColParam %in% c("gtype", "hardness", "density", "temp", "gsize", "ssi")))
    stop(paste("ColParam", ColParam, "not supportd"))


  ## ---- Filter and sort profiles ----

  ## Calculate metadata
  Metadata <- summary(Profiles)

  ## Filter dates
  if (SortMethod == "time") {
    ## Filter date range
    Drop <- c(which(Metadata$date < DateStart), which(Metadata$date > DateEnd))
    if (length(Drop) > 0) {
      Profiles <- Profiles[-Drop]
      if (length(Profiles) == 0)
        stop("No profiles between DateStart and DateEnd")
      Metadata <- Metadata[-Drop, ]
    }
  }

  ## Sort profiles
  if (SortMethod == "time") {
    k <- order(Metadata$date)
  } else if (SortMethod == "hs") {
    k <- order(Metadata$hs)
  } else if (SortMethod == "elev") {
    k <- order(Metadata$elev)
  } else if (SortMethod == "unsorted") {
    k <- seq(length(Profiles))
  }
  Profiles <- Profiles[k]

  ## ---- Calculate xy positions of layers ----

  ## Rbind all the profiles to get table with all layers
  Lyrs <- rbind(Profiles)

  ## X values (index or date) Calculate index for each unique profile
  Lyrs$index <- cumsum(!duplicated(Lyrs[c("station_id", "datetime")]))
  xx <- Lyrs$index
  if (SortMethod == "time") {
    xx <- Lyrs$date
    if (any(duplicated(Metadata$date)))
      warning("Multiple profiles exist with same date. Since SortMethod = 'time' profiles are being plotted on top of each other on these date(s).")
  }

  ## Y-axis values (layer height/depths) Default draw rectangles with tops at 'height' and bottoms at lower interface
  y2 <- Lyrs$height
  y1 <- c(0, Lyrs$height[1:(nrow(Lyrs) - 1)])
  y1[which(!duplicated(Lyrs$index))] <- 0

  ## Transform coordinates for TopDown profiles
  if (TopDown) {
    y2 <- -Lyrs$depth
    y1 <- c(0, y2[1:(nrow(Lyrs) - 1)])
    y1[which(!duplicated(Lyrs$index))] <- 0
    y1[which(!duplicated(Lyrs$index))] <- -Lyrs$hs[which(!duplicated(Lyrs$index))]
  }


  ## ---- Select colour scheme ----

  if (ColParam == "gtype") {
    Lyrs$Col <- getColoursGrainType(Lyrs$gtype)
  } else if (ColParam == "hardness") {
    Lyrs$Col <- getColoursHardness(Lyrs$hardness)
  } else if (ColParam == "density") {
    Lyrs$Col <- getColoursDensity(Lyrs$density)
  } else if (startsWith(ColParam, "temp")) {
    Lyrs$Col <- getColoursSnowTemp(Lyrs$temperature)
  } else if (ColParam == "gsize") {
    Lyrs$Col <- getColoursGrainSize(Lyrs$gsize)
  } else {
    Lyrs$Col <- "#E8E8E8"
  }


  ## ---- Set plot dimensions ----

  ## xlim
  xlim <- c(0.5 - xPadding, length(Profiles) + 0.5 + xPadding)
  if (SortMethod == "time")
    xlim <- c(min(xx) - 1, max(xx + 1))

  ## ylim
  if (is.null(ylim)) {
    ymin <- ifelse(TopDown, -max(Metadata$hs) - yPadding, 0)
    ymax <- ifelse(TopDown, 0, max(Metadata$hs) + yPadding)
    ylim <- c(ymin, ymax)
  }


  ## ---- Draw plot ----

  ## Initialize empty plot
  plot(NA, NA, type = "n",
       xlim = xlim, ylim = ylim,
       xaxt = "n", yaxt = "n", axes = FALSE,
       xlab = "", ylab = "",
       ...)

  ## Draw rectangles
  rect(xleft = xx - 0.5,
       ybottom = y1,
       xright = xx + 0.5,
       ytop = y2,
       col = Lyrs$Col,
       border = ifelse(OutlineLyrs == FALSE, NA, "#606060"))


  ## ---- Grids and labels ----

  ## Labels
  title(xlab = ifelse(is.na(xlab), ifelse(SortMethod == "hs", expression("Thinnest snowpack" %<->% "Thickest snowpack"), ""), xlab),
        ylab = ifelse(is.na(ylab), ifelse(TopDown, "Depth (cm)", "Height (cm)"), ylab),
        main = ifelse(is.na(main), "", main))

  ## Date labels
  if (labelOriginalIndices) axis(1, at = 1:length(Profiles), labels = k)
  if (SortMethod == "time") {
    DateArray <- seq(xlim[1], xlim[2], by = "days")
    Saturdays <- DateArray[weekdays(DateArray, abbreviate = FALSE) == "Saturday"]
    SaturdayLabels <- format(DateArray[weekdays(DateArray, abbreviate = FALSE) == "Saturday"], "%b %d")
    abline(v = Saturdays, lty = 3, col = "dark grey")
    axis(1, at = Saturdays, labels = SaturdayLabels)
  }

  ## Height grid and y-axis labels
  HeightGrid <- pretty(ylim, n = 4)
  if (TopDown) {
    axis(2, at = HeightGrid, labels = -HeightGrid)
  } else {
    axis(2, at = HeightGrid, labels = HeightGrid)
  }
  if (HorizGrid) abline(h = HeightGrid, lty = 3, col = "dark grey")

  ## Draw box around plot
  if (box) box()

}

