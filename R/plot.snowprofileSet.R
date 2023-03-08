#' Plot a single layer property in multiple profiles side-by-side
#'
#' A flexible function to plot multiple snowprofiles either in a timeseries or various types of groups.
#'
#' The routine allows you to plot coloured sequences only, or to include hardness profile information as well. See parameter
#' `hardnessResidual` and the examples for more details.
#' To change the font size of labels etc, use `par()` with the parameters `cex.lab`, `cex.axis`, etc.
#'
#' @param x An object of class [snowprofileSet]
#' @param SortMethod How to arrange profiles along the x-axis. Options include timeseries (default = 'time'), in existing order of Profiles list ('unsorted'), sorted by HS ('hs'), or elevation ('elev')
#' @param ColParam What parameter to show with colour. So far the following types are available: "gtype", "hardness", "density", "temp", "gsize", "ssi", "p_unstable", "crit_cut_length", "rta", "percentage".
#' @param TopDown Option to plot by depth instead of height with zero depth on top of plot (default = FALSE)
#' @param DateStart Start date for timeseries plots (`SortMethod = 'time'`). If not provided, the function takes the date range from Profiles (default = NA).
#' @param DateEnd End date for timeseries plots (`SortMethod = 'time'`). If not provided, the function takes the date range from Profiles (default = NA).
#' @param Timeseries_labels Label Saturdays "weekly", "monthly", or `NA`
#' @param ylim Vertical range of plot
#' @param OutlineLyrs Switch for outlining layers (default = FALSE)
#' @param emphasizeLayers emphasize layers with different transparency than others, or a different color altogether? then set this argument to `TRUE` if you want to emphasize all
#' labeled layers of interest (aka weak layers), or provide a named list with arguments to a function call to [findPWL] to define which layers to emphasize.
#' Set either `colAlpha` or `colEmphasis` to make the emphasis apparent.
#' @param colAlpha the transparency setting for all layers (except the ones to be emphasized if you want to emphasize any). This can be useful for example
#' if you want to overplot the grain type sequences with another variable, e.g. a percentage from a distribution.
#' @param colEmphasis the color of the layers to be emphasized (only if you want a different color than defined by `ColParam`)
#' @param OutlineProfile vector of profile indices that will be outlined to highlight them
#' @param HorizGrid Draw horizontal grid at layer heights (default = TRUE)
#' @param VerticalGrid Draw vertical grid at xticks (default = TRUE)
#' @param yaxis draw a y-axis? (either `FALSE`, `TRUE` draws yaxis left, `"right"` draws yaxis on the right plot side)
#' *Note* that in case of `"right"` you need to adjust `par(mar = ...)`, disable `ylab` and manually draw an xlab with `mtext`.
#' @param main Main title
#' @param ylab y-axis label; disable ylab by providing an empty string (i.e., ylab = '')
#' @param xlab x-axis label; disable xlab by providing an empty string (i.e., xlab = '')
#' @param box Draw a box around the plot (default = TRUE)
#' @param xticklabels Label the profiles with their "names", "originalIndices" (prior to sorting), "dates", or a custom character array
#' @param xtick.las Orientation of labels if xticklabels is specified.
#' @param yPadding Padding between ylim and limits of data, default = 10.
#' Note that R will still put padding by default. If you want to prohibit that entirely, specify `xaxs ='i'`, or `yaxs = 'i'`.
#' @param xPadding Padding between xlim and limits of data, default = 0.5.
#' Note that R will still put padding by default. If you want to prohibit that entirely, specify `xaxs = 'i'`, or `yaxs = 'i'`. For xPadding, you can provide either a scalar, or
#' a length 2 numeric for left and right hand side, respectively.
#' @param hardnessResidual Value within `[0, 1]` to control the minimum horizontal space of each layer that will be colored
#' irrespective of the layer's hardness. A value of `1` corresponds to no hardness being shown.
#' @param hardnessScale A scaling factor that exaggerates the hardness profile to subsequent cells on the x-axis. Useful
#' for time series of sparse profile observations. Note that this scaling factor is unused when `hardnessScale = 1` and that
#' it gets more influential the smaller `hardnessScale` gets. Also note, that a `hardnessScale > 1` can lead to profiles overlapping.
#' @param hardnessOffset offsets the profile location on the x-axis
#' @param k a sorting vector if `SortMethod = "presorted"`.
#' @param offset Provide a Date or POSIXct offset if you want to offset the vertical snow height/depth axis so that the offset date aligns with snow depth/height 0.
#' @param add add the plot to an existing plot, or create new plot?
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
#' ## Show hardness profile, too:
#' plot(SPtimeline, hardnessResidual = 0.5)
#'
#' ## Additional examples of plot dimensions and labelling
#' ## Label the indices of the profiles in the list:
#' plot(SPgroup, SortMethod = 'elev', xticklabels = "originalIndices")
#' ##  ... and with minimized axis limits and their station ID names:
#' plot(SPgroup, SortMethod = 'elev', xticklabels = sapply(SPgroup, function(x) x$station_id),
#'        yPadding = 0, xPadding = 0, xaxs = 'i', yaxs = 'i')
#' ##  sorted by depth, and without box:
#' plot(SPgroup, SortMethod = 'hs', TopDown = TRUE, box = FALSE)
#'
#' ## Apply a date offset to investigate which layers formed around that day of interest:
#' pwl_exists <- sapply(SPgroup, function(sp)
#'   {length(findPWL(sp, pwl_date = "2019-01-21", pwl_gtype = c("SH", "DH"),
#'                   date_range_earlier = as.difftime(2, unit = "days"))) > 0})
#' k <- order(pwl_exists, decreasing = TRUE)
#' plot(SPgroup, SortMethod = 'presorted', k = k, xticklabels = "originalIndices",
#'      offset = as.Date("2019-01-21"), xlab = "<-- Jan 21 PWL exists | does not exist -->")
#' abline(v = max(which(pwl_exists[k]))+0.5, lty = "dashed")
#'
#' ## Emphasize specific layers
#' ## (i) all labeled layers of interest:
#' SPgroup <- snowprofileSet(lapply(SPgroup, labelPWL))  # label layers with default settings
#' plot(SPgroup, SortMethod = "hs", emphasizeLayers = TRUE, colAlpha = 0.3)
#' ## (ii) specific individual layers:
#' plot(SPgroup, SortMethod = "hs",
#'      emphasizeLayers = list(pwl_gtype = c("SH", "DH"), pwl_date = "2019-01-21"),
#'      colAlpha = 0.3, colEmphasis = "black")
#'
#'
#' @export
#'

plot.snowprofileSet <- function(x,
                                SortMethod = c("time", "unsorted", "hs", "elev", "presorted"),
                                ColParam = c("gtype", "hardness", "density", "temp", "gsize", "ssi", "p_unstable", "crit_cut_length", "rta", "percentage"),
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
                                ...) {


  ## ---- Check input parameters ----

  ## Profiles
  Profiles <- x
  if (!inherits(Profiles, 'snowprofileSet'))
    stop("plot.snowprofileSet requires an object of class snowprofileSet")

  ## SortMethod
  if (!(SortMethod[1] %in% c("time", "hs", "elev", "unsorted", "presorted")))
    stop(paste("SortMethod", SortMethod, "not supported ('time', 'hs', 'elev', 'unsorted')"))

  ## ColParam
  if (!(ColParam[1] %in% c("gtype", "hardness", "density", "temp", "gsize", "ssi", "p_unstable", "crit_cut_length", "rta", "percentage")))
    stop(paste("ColParam", ColParam, "not supported"))


  ## ---- Filter and sort profiles ----

  ## Calculate metadata
  Metadata <- summary(Profiles, fast = FALSE)

  ## Filter dates
  if (SortMethod[1] == "time") {
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
  if (SortMethod[1] == "time") {
    k <- order(Metadata$date)
  } else if (SortMethod[1] == "hs") {
    k <- order(Metadata$hs)
  } else if (SortMethod[1] == "elev") {
    k <- order(Metadata$elev)
  } else if (SortMethod[1] == "unsorted") {
    k <- seq(length(Profiles))
  } else if (SortMethod[1] == "presorted") {
    if (is.null(k)) stop("SortMethod 'presorted', but k is not provided!")
    if (length(k) != length(Profiles)) stop("k has a different length than your number of snowprofiles!")
  }
  Profiles <- Profiles[k]
  Metadata <- Metadata[k, ]
  if (!is.null(OutlineProfile)) OutlineProfile <- sapply(OutlineProfile, function(op) which(k == op))

  ## ---- Calculate xy positions of layers ----

  ## offset layer locations based on a given offset Date
  if (!is.na(offset)) {
    # tz_used <- NA
    # try({tz_used <- attr(Profiles[[1]]$layers$datetag[1], 'tz')})
    # try({tz_used <- attr(Profiles[[1]]$layers$ddate[1], 'tz')})
    # if (inherits(offset, "Date")) {
    #   if (is.na(tz_used)) {
    #     offset <- as.POSIXct(offset)
    #   } else {
    #     offset <- as.POSIXct(offset, tz = tz_used)
    #   }
    # } else if (!inherits(offset, "POSIXct")) stop("offset needs to be of class Date or POSIXct!")
    if (inherits(offset, "POSIXct")) {
      offset <- as.Date(as.character(offset))
    } else if (!inherits(offset, "Date")) stop("offset needs to be of class Date or POSIXct!")

    offset_profiles <- lapply(Profiles, function(sp) {
      sp_d <- deriveDatetag(sp)
      idx_offset <- which.min(abs(sp_d$layers$datetag - offset))
      sp_d$layers$height <- sp_d$layers$height - sp_d$layers$height[idx_offset]
      return(sp_d)
    })
    Profiles <- snowprofileSet(offset_profiles)
  }

  ## compute which layers to emphasize (if desired)
  if (!all(is.null(emphasizeLayers))) {
    emphasizeLayersofinterest <- FALSE
    if (isTRUE(emphasizeLayers)) {
      emphasizeLayersofinterest <- TRUE
    } else if (is.list(emphasizeLayers)) {
      Profiles <- snowprofileSet(lapply(Profiles, function(sp) {
        sp$layers$toEmphasize <- FALSE
        sp$layers$toEmphasize[do.call("findPWL", c(quote(sp), emphasizeLayers))] <- TRUE
        sp
      }))
    }
    if (is.na(colAlpha) & is.na(colEmphasis)) warning("If you want to emphasizeLayers, you also need to specify a colAlpha or colEmphasis to make the emphasis apparent!")
  }

  ## Rbind all the profiles to get table with all layers
  Lyrs <- rbind(Profiles)

  ## X values (index or date) Calculate index for each unique profile
  internal_idx <- unlist(sapply(seq_along(Metadata$nLayers), function(i) rep(i, times = Metadata$nLayers[i])))
  Lyrs$index <- cumsum(!duplicated(Lyrs[c("station_id", "datetime")]))
  if (length(unique(Lyrs$index)) < nrow(Metadata)) Lyrs$index <- as.vector(internal_idx)
  xx <- Lyrs$index
  if (SortMethod[1] == "time") {
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

  if (ColParam[1] == "gtype") {
    Lyrs$Col <- getColoursGrainType(Lyrs$gtype)
  } else if (ColParam[1] == "hardness") {
    Lyrs$Col <- getColoursHardness(Lyrs$hardness)
  } else if (ColParam[1] == "density") {
    Lyrs$Col <- getColoursDensity(Lyrs$density)
  } else if (startsWith(ColParam[1], "temp")) {
    Lyrs$Col <- getColoursSnowTemp(Lyrs$temperature)
  } else if (ColParam[1] == "gsize") {
    Lyrs$Col <- getColoursGrainSize(Lyrs$gsize)
  } else if (ColParam[1] == "p_unstable") {
    Lyrs$Col <- getColoursStability(Lyrs$p_unstable, StabilityIndexThreshold = 0.77, StabilityIndexRange = c(0, 1))
  } else if (ColParam[1] == "crit_cut_length") {
    Lyrs$Col <- getColoursStability(Lyrs$crit_cut_length, StabilityIndexThreshold = 0.4, StabilityIndexRange = c(0, 3), invers = TRUE)
  } else if (ColParam[1] == "rta") {
    Lyrs$Col <- getColoursStability(Lyrs$rta, StabilityIndexThreshold = 0.8, StabilityIndexRange = c(0, 1))
  } else if (ColParam[1] == "percentage") {
    Lyrs$Col <- getColoursPercentage(Lyrs$percentage, ClrRamp = "Greys_transparent")
  } else {
    Lyrs$Col <- "#E8E8E8"
  }

  if (!is.na(colAlpha) | !is.na(colEmphasis)) {
    if (!all(is.null(emphasizeLayers))) {
      if (!is.na(colEmphasis)) {
        Lyrs$emphasisCol <- colEmphasis
      } else {
        Lyrs$emphasisCol <- Lyrs$Col
      }
    }
    if (!is.na(colAlpha)) Lyrs$Col <- sapply(Lyrs$Col, function(cl) adjustcolor(cl, alpha.f = colAlpha))
  }

  ## ---- Set plot dimensions ----

  ## xlim
  if (length(xPadding) == 2) xlim <- c(0.5 - xPadding[1], length(Profiles) + 0.5 + xPadding[2]) else xlim <- c(0.5 - xPadding, length(Profiles) + 0.5 + xPadding)
  if (SortMethod[1] == "time") {
    if (length(xPadding) == 2) xlim <- c(min(xx) - xPadding[1], max(xx + xPadding[2])) else xlim <- c(min(xx) - xPadding, max(xx + xPadding))
    if (any(is.na(xlim))) stop("Profiles do not contain date information, SortMethod 'time' is not available, try SorthMethod = 'unsorted'")
  }

  ## ylim
  if (is.null(ylim)) {
    if (!is.na(offset)) {
      ylim <- c(min(y1), max(y2))
    } else {
      getMaxHSequivalent <- function(Metadata) {
        offset <- suppressWarnings(max(Metadata$hs, na.rm = TRUE))
        if (is.infinite(offset)) offset <- max(Metadata$maxObservedDepth, na.rm = TRUE)
        return(offset)
      }
      ymin <- ifelse(TopDown, -getMaxHSequivalent(Metadata) - yPadding, 0)
      ymax <- ifelse(TopDown, 0, getMaxHSequivalent(Metadata) + yPadding)
      ylim <- c(ymin, ymax)
    }
  } else if (is.call(ylim)) ylim <- eval(ylim)  # allows users to send quoted ylims, e.g. quote(c(max(min(y1), -100), min(100, max(y2))))


  ## ---- Draw plot ----

  ## Initialize empty plot
  if (!add) {
    plot(NA, NA, type = "n",
         xlim = xlim, ylim = ylim,
         xaxt = "n", yaxt = "n", axes = FALSE,
         xlab = "", ylab = "",
         ...)
  }


  if (exists("offset_profiles")) abline(h = 0, lwd = 1.5)

  ## Prepare hardness scaling:
  Hardness <- Lyrs$hardness / 5  # hardness values relative to "Knife"
  Hardness[which(is.na(Hardness) | Hardness == 0)] <- 0
  if (length(Hardness) < length(xx)) Hardness <- rep(0, times = length(xx))

  ## Draw profiles by creating individual layer rectangles
  rect(xleft = xx + hardnessOffset,
       ybottom = y1,
       xright = xx + hardnessOffset + hardnessResidual + Hardness*hardnessScale*(1-hardnessResidual),
       ytop = y2,
       col = Lyrs$Col,
       border = ifelse(OutlineLyrs == FALSE, NA, "#606060"))

  ## Outline individual profiles by creating individual rectangles
  if (!is.null(OutlineProfile)) {
    rect(xleft = OutlineProfile + hardnessOffset,
         ybottom = sapply(OutlineProfile, function(op) min(y1[xx %in% op])),
         xright = OutlineProfile + hardnessOffset + hardnessResidual + 1*hardnessScale*(1-hardnessResidual),
         ytop = sapply(OutlineProfile, function(op) max(y2[xx %in% op])),
         col = NA,
         border = "#606060")
  }

  ## Emphasize specific, individual layers
  if (!all(is.null(emphasizeLayers))) {
    if (emphasizeLayersofinterest) {
      rect(xleft = xx[Lyrs$layerOfInterest] + hardnessOffset,
           ybottom = y1[Lyrs$layerOfInterest],
           xright = xx[Lyrs$layerOfInterest] + hardnessOffset + hardnessResidual + Hardness[Lyrs$layerOfInterest]*hardnessScale*(1-hardnessResidual),
           ytop = y2[Lyrs$layerOfInterest],
           col = Lyrs$emphasisCol[Lyrs$layerOfInterest],
           border = ifelse(OutlineLyrs == FALSE, NA, "#606060"))
    } else if ("toEmphasize" %in% names(Lyrs)) {
      rect(xleft = xx[Lyrs$toEmphasize] + hardnessOffset,
           ybottom = y1[Lyrs$toEmphasize],
           xright = xx[Lyrs$toEmphasize] + hardnessOffset + hardnessResidual + Hardness[Lyrs$toEmphasize]*hardnessScale*(1-hardnessResidual),
           ytop = y2[Lyrs$toEmphasize],
           col = Lyrs$emphasisCol[Lyrs$toEmphasize],
           border = ifelse(OutlineLyrs == FALSE, NA, "#606060"))
    }
  }


  ## ---- Grids and labels ----

  ## Labels
  title(xlab = ifelse(is.na(xlab), ifelse(SortMethod == "hs", expression("Thinnest snowpack" %<->% "Thickest snowpack"), ""), xlab),
        ylab = ifelse(is.na(ylab), ifelse(TopDown, "Depth (cm)", "Height (cm)"), ylab),
        main = ifelse(is.na(main), "", main))

  ## Date labels
  if (all(xticklabels == "originalIndices")) {
    axis(1, at = 1:length(Profiles), labels = k, las = xtick.las)
  } else if (all(xticklabels == "names")) {
    axis(1, at = 1:length(Profiles), labels = names(x)[k], las = xtick.las)
  } else if (length(xticklabels) > 1) {
    axis(1, at = 1:length(Profiles), labels = xticklabels[k], las = xtick.las)
  } else if (all(xticklabels == "dates")) {
    if (!SortMethod[1] == "time") stop("xticklabels 'date' are currently only implemented for SortMethod 'time'.")
    axis(1, at = unique(xx), labels = format(unique(xx), "%b %d"), las = xtick.las)
  }

  if (SortMethod[1] == "time") {
    if (!is.na(Timeseries_labels[1])) {

      DateArray <- seq(xlim[1], xlim[2], by = "days")
      Saturdays <- DateArray[weekdays(DateArray, abbreviate = FALSE) == "Saturday"]
      SaturdayLabels <- format(DateArray[weekdays(DateArray, abbreviate = FALSE) == "Saturday"], "%b %d")
      if (Timeseries_labels[1] == "monthly") {
        Saturdays <- Saturdays[seq(1, length(Saturdays), 4)]
        SaturdayLabels <- SaturdayLabels[seq(1, length(SaturdayLabels), 4)]
      }
      if (VerticalGrid) abline(v = Saturdays, lty = 3, col = "dark grey")
      axis(1, at = Saturdays, labels = SaturdayLabels, las = xtick.las)
    }
  }

  ## Height grid and y-axis labels
  if (isTRUE(yaxis) || yaxis == "right") {
    if (yaxis == "right") yaxisLoc <- 4
    else yaxisLoc <- 2
    HeightGrid <- pretty(ylim, n = 4)
    if (TopDown) {
      axis(yaxisLoc, at = HeightGrid, labels = -HeightGrid)
    } else {
      axis(yaxisLoc, at = HeightGrid, labels = HeightGrid)
    }
    if (HorizGrid) abline(h = HeightGrid, lty = 3, col = "dark grey")
  }


  ## Draw box around plot
  if (box) box()

}

