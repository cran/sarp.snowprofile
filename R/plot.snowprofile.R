#' Plot hardness profile
#'
#' @param x [snowprofile] object
#' @param TempProfile draw unscaled temperature profile (default = TRUE)? Temperature data needs to be included in the
#' snowprofile object either under `x$layers$temperature`, or in a separate `x$temperatureProfile` data.frame providing
#' a vertical grid independent from the snow layers.
#' @param xlimTemp the x limits in degrees Celsius for the temperature profile (if left empty it scales to the range of temperature values)
#' @param Col vector of colours corresponding to the grain types in the profile (defaults to a lookup table)
#' @param TopDown Option to plot by depth instead of height with zero depth on top of plot (default = FALSE)
#' @param axes Should axes be printed?
#' @param xlab x-axis label, defaults to an empty string
#' @param emphasizeLayers index OR character vector (grain types) of layers to be emphasized (i.e. all other layers become slightly transparent)
#' @param emphasis 2 digit quoted number between `'01'`-`'99'` to control the degree of emphasis; the higher the stronger
#' @param failureLayers height vector of failure layers that will be indicated with an arrow
#' @param failureLayers.cex factor to shrink or enlarge the arrow
#' @param failureLayers.col color of arrow, can also be a vector of same length as `failureLayers` to color different arrows differently
#' @param nYTicks number of tick marks at yaxis
#' @param ymax the maximum ylim value
#' @param alignWithBottomUpPlot useful when aligning the yaxis grids of bottom up profileSet plots and top down hardness plots.
#' @param highlightUnobservedBasalLayers draw sine wave at lowest observed layer to highlight unobserved layers below
#' @param label.datetags label the datetags of the snowprofile layers? (Won't produce a pretty plot, but give you some more information for analysis)
#' @param ... other parameters to barplot
#'
#' @seealso [plot.snowprofileSet]
#'
#' @examples
#'
#' plot(SPpairs$A_manual)
#' plot(SPpairs$A_manual, Col = 'black')
#' plot(SPpairs$A_manual, emphasizeLayers = c(5, 11),
#'      failureLayers = SPpairs$A_manual$layers$height[5], failureLayers.cex = 1.5)
#' plot(SPpairs$A_manual, emphasizeLayers = 'SH')
#' plot(SPpairs$A_manual, TopDown = TRUE)
#' plot(SPpairs$A_modeled, TempProfile = TRUE, xlimTemp = c(-30,10))
#'
#' # highlight unobserved basal layers:
#' plot(snowprofile(layers = snowprofileLayers(depth = c(40, 25, 0),
#'                                                hardness = c(2, 3, 1),
#'                                                gtype = c('FC', NA, 'PP'),
#'                                                hs = 70,
#'                                                maxObservedDepth = 50)), TopDown = TRUE, ymax = 80)
#'
#' @export
#'
plot.snowprofile <- function(x,
                             TempProfile = TRUE,
                             xlimTemp = NULL,
                             Col = 'auto',
                             TopDown = 'auto',
                             axes = TRUE,
                             xlab = "",
                             emphasizeLayers = FALSE,
                             emphasis = "95",
                             failureLayers = FALSE,
                             failureLayers.cex = 1,
                             failureLayers.col = "red",
                             nYTicks = 4,
                             ymax = max(c(x$maxObservedDepth, x$hs), na.rm = TRUE),
                             alignWithBottomUpPlot = FALSE,
                             highlightUnobservedBasalLayers = TRUE,
                             label.datetags = FALSE,
                             ...) {

  ## Rename generic input x
  Profile <- x

  ## Extract snowprofile layers
  Layers <- Profile$layers

  ## allow for automatic BottomUp / TopDown plots:
  if (TopDown == "auto") {
    if (is.na(Profile$hs)) TopDown <- TRUE
    else TopDown <- FALSE
  }
  heightshift <- NA
  if (TopDown) {
    if (ymax > max(Layers$height) && alignWithBottomUpPlot == FALSE) {
      heightshift <- ymax - max(Layers$height)
      Layers <- insertUnobservedBasalLayer(Layers, heightshift, setBasalThicknessNA = FALSE)
    }
  }

  ## extract dots and issue warning if TopDown and ylim
  dots <- list(...)
  if ("ylim" %in% names(dots)) {
    warning("Can't specify ylim, use ymax instead")
  }

  # Get colors:
  if (all(Col == "auto")) Col <- sapply(Layers$gtype, getColoursGrainType)
  if (!("gtype" %in% names(Layers)) & length(Col) == 0) Col <- rep("gray", times = nrow(Layers))
  if (!("hardness" %in% names(Layers))) stop("Snowprofile missing harndess values")

  ## Draw horizontal barplot with hardness profile
  barplot(Layers$hardness,
          width = c(Layers$height[1], diff(Layers$height)),
          col = Col,
          horiz = TRUE,
          border = NA,
          space = 0,
          xlim = c(0, 5),
          xaxt = "n",
          xlab = xlab,
          ylim = c(0, ymax),
          ...)

  ## Emphasize layers (by over-drawing all other layers with a white transparent layer)
  if (!isFALSE(emphasizeLayers)) {
    if (is.character(emphasizeLayers)) emphasizeLayers <- which(Layers$gtype %in% emphasizeLayers)
    else if (!is.na(heightshift)) emphasizeLayers <- emphasizeLayers + 1  # shift index b/c of insertedUnobservedBasalLayer
    hardness_mod <- Layers$hardness
    hardness_mod[emphasizeLayers] <- 0
    barplot(hardness_mod,
            width = c(Layers$height[1], diff(Layers$height)),
            col = rep(paste0("#FFFFFF", emphasis), times = length(hardness_mod)),
            horiz = TRUE,
            border = NA,
            space = 0,
            xlim = c(0, 5),
            xaxt = "n",
            xlab = "",
            ylim = c(0, ymax),
            add = TRUE)
  }

  ## Draw scaled temperature profile
  if (TempProfile) {
    ## check where to find temperature data
    if ("temperature" %in% names(Layers)) {
      temps <- Layers$temperature
      tempsHeight <- Layers$height
    } else if ("temperatureProfile" %in% names(Profile)) {
      temps <- Profile$temperatureProfile$temperature
      tempsHeight <- Profile$temperatureProfile$height
    } else {
      temps <- c()
    }
    if (length(temps) > 0) {
      if (is.null(xlimTemp)) xlimTemp <- range(temps)
      ## hard code for isothermal profile
      if (all(temps == 0)) {
        xlimTemp <- c(-1, 0)
        tempsScaled <- c(2, 2)
        tempsHeight <- range(tempsHeight)
      ## scale temperature to plot
      } else {
        x1 <- (min(temps) - xlimTemp[1])/(xlimTemp[2] - xlimTemp[1])
        x2 <- (max(temps) - xlimTemp[1])/(xlimTemp[2] - xlimTemp[1])
        t_per <- (temps - min(temps))/(max(temps) - min(temps))
        tempsScaled <- (t_per*(x2 - x1) + x1) * 2
      }
      ## draw temperature and axis
      lines(tempsScaled, tempsHeight, col = "black", lwd = 2)
      axis(3, seq(0, 2, length.out = length(pretty(xlimTemp))), pretty(xlimTemp))
    }
  }

  ## Draw arrow to indicate failure layers
  if (!isFALSE(failureLayers)) {
    if (!is.na(heightshift)) failureLayers <- failureLayers + heightshift  # shift failureLayers height b/c of insertedUnobservedBasalLayer
    for (i in seq(length(failureLayers))) {
      if (length(failureLayers.col) > 1) {
        if (length(failureLayers.col) != length(failureLayers)) stop("failureLayers.col must either be one color or as many as failureLayers!")
        arrows(-0.09 * failureLayers.cex, failureLayers[i], 0, failureLayers[i],
               col = failureLayers.col[i], lwd = 2.5, length = 0.07 * sqrt(failureLayers.cex), xpd = TRUE)
      } else {
        arrows(-0.09 * failureLayers.cex, failureLayers[i], 0, failureLayers[i],
               col = failureLayers.col, lwd = 2.5, length = 0.07 * sqrt(failureLayers.cex), xpd = TRUE)
      }
    }
  }

  ## Add harndess and height/depth axis
  if (axes) {
    axis(1, at = 1:5, labels = c("F", "4F", "1F", "P", "K"))

    if (TopDown) {
      DepthGrid <- pretty(c(0, ymax), n = nYTicks)
      HeightGrid <- ymax - DepthGrid
      if (alignWithBottomUpPlot) {
        DepthGrid <- pretty(DepthGrid - (ymax - max(Layers$height)), n = nYTicks)
        DepthGrid <- DepthGrid[DepthGrid >= 0]
        HeightGrid <- ymax - (DepthGrid + ymax - max(Layers$height))

      }
      axis(2, at = HeightGrid, labels = DepthGrid)
    } else {
      HeightGrid <- pretty(c(0, ymax), n = nYTicks)
      axis(2, at = HeightGrid)
    }
  }

  ## Add sine wave to highlight unobserved basal layers
  if (highlightUnobservedBasalLayers) {
    if ((is.na(Profile$hs)) || isTRUE(Profile$maxObservedDepth < (Profile$hs - 5))) {  # 5 cm tolerance
      t=seq(0.5, 1.5, 0.01)
      sint <- 0.7 * sin(pi*t*10-pi/2) + Layers$height[1 + ifelse(is.na(heightshift), 0, 1)]
      lines(t, sint)
    }
  }

  ## label datetags if present
  if (label.datetags) {
    if (!"datetag" %in% names(Layers)) suppressWarnings( Layers <- deriveDatetag(Layers) )

    labelsText = as.Date(Layers$datetag)
    ispaced <- which(!duplicated(labelsText))
    labelsText <- labelsText[ispaced]
    xText <- matrix(1, nrow = nrow(Layers), ncol = 1)[ispaced]
    yText <-  (Layers$height - 0.5 * diff(c(0, Layers$height)))[ispaced]
    text(xText, yText, labelsText)
  }


}
