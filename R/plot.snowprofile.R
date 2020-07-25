#' Plot hardness profile
#'
#' @param x [snowprofile] object
#' @param TempProfile draw unscaled temperature profile (default = TRUE)
#' @param Col vector of colours corresponding to the grain types in the profile (defaults to a lookup table)
#' @param TopDown Option to plot by depth instead of height with zero depth on top of plot (default = FALSE)
#' @param axes Should axes be printed?
#' @param xlab x-axis label, defaults to an empty string
#' @param emphasizeLayers index OR character vector (grain types) of layers to be emphasized (i.e. all other layers become slightly transparent)
#' @param emphasis 2 digit quoted number between `'01'`-`'99'` to control the degree of emphasis; the higher the stronger
#' @param failureLayers height vector of failure layers that will be indicated with a red arrow
#' @param failureLayers.cex factor to shrink or enlarge the arrow
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
#'
#' @export
#'
plot.snowprofile <- function(x,
                             TempProfile = TRUE,
                             Col = sapply(x$layers$gtype, getColoursGrainType),
                             TopDown = FALSE,
                             axes = TRUE,
                             xlab = "",
                             emphasizeLayers = FALSE,
                             emphasis = "95",
                             failureLayers = FALSE,
                             failureLayers.cex = 1,
                             ...) {

  ## Rename generic input x
  Profile <- x

  ## Extract snowprofile layers
  Layers <- Profile$layers

  if (!("hardness" %in% names(Layers)))
    stop("Snowprofile missing harndess values")

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
          ...)

  ## Emphasize layers (by over-drawing all other layers with a white transparent layer)
  if (!isFALSE(emphasizeLayers)) {
    if (is.character(emphasizeLayers)) emphasizeLayers <- which(Layers$gtype %in% emphasizeLayers)
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
            add = TRUE)
  }

  ## Draw scaled temperature profile
  if (TempProfile) {
    if ("temperature" %in% names(Layers)) {
      lines(Layers$temperature/(max(Layers$temperature) - min(Layers$temperature) + 0.1) * 2 + 2,
            Layers$height,
            col = "red")
    }
  }

  ## Draw arrow to indicate failure layers
  if (!isFALSE(failureLayers)) {
    for (i in seq(length(failureLayers))) {
      arrows(-0.09 * failureLayers.cex, failureLayers[i], 0, failureLayers[i],
             col = "red", lwd = 2.5, length = 0.1 * sqrt(failureLayers.cex), xpd = TRUE)
    }
  }

  ## Add harndess and height/depth axis
  if (axes) {
    axis(1, at = 1:5, labels = c("F", "4F", "1F", "P", "K"))

    if (TopDown) {
      DepthGrid <- pretty(c(0, Profile$hs), n = 4)
      HeightGrid <- Profile$hs - DepthGrid
      axis(2, at = HeightGrid, labels = DepthGrid)
    } else {
      axis(2)
    }
  }

}
