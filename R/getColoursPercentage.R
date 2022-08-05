#' Gets colours for plotting the snow layer property 'percentage'
#'
#' Gets colours for plotting the snow layer property 'percentage', as used for example for distributions from 0--1.
#'
#'
#' @param Values of the 'percentage' variable
#' @param Resolution Resolution of colour scale. Default is 100.
#' @param Min Minimum values of the percentage (for colouring)
#' @param Max Maximum --=--
#' @param ClrRamp Three different colourmaps can be chosen from: "Blues", "Greys", "Greys_transparent"
#'
#' @return Array with HTML colour codes
#'
#' @author fherla
#'
#' @seealso [getColoursGrainSize], [getColoursGrainType], [getColoursHardness], [getColoursLWC], [getColoursSnowTemp], [getColoursStability]
#'
#' @examples
#'
#' prct <- seq(0, 1, by=0.1)
#' plot(x = rep(1,length(prct)), y = prct,
#'      col = getColoursPercentage(prct), pch = 19, cex = 3)
#'
#' plot(x = rep(1,length(prct)), y = prct,
#'      col = getColoursPercentage(prct, ClrRamp = "Greys"), pch = 19, cex = 3)
#'
#'
#' @export
getColoursPercentage <- function(Values, Resolution = 101, Min = 0, Max = 1, ClrRamp = c("Blues", "Greys", "Greys_transparent")[1]) {


  if (ClrRamp == "Blues") {
    ClrPalette <- colorRampPalette(c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#756bb1", "#54278f"))
    ClrRamp <- ClrPalette(Resolution)
  } else if (ClrRamp == "Greys") {
    ClrPalette <- colorRampPalette(c('#F7F7F7', '#CCCCCC', '#969696', '#636363', '#252525'))
    ClrRamp <- ClrPalette(Resolution)
  } else if (ClrRamp == "Greys_transparent") {
    Resolution <- max(Resolution, 100)
    ClrRamp <- sapply(seq(0, 1, length.out = Resolution), function(alph) adjustcolor("black", alpha.f = alph))
  }



  cutval <- cut(Values, breaks = seq(Min, Max, len = Resolution))

  # Cols <- rep("#d9d9d9", length(Values))  # initialize with gray col
  Cols <- ClrRamp[as.numeric(cutval)]

  return(Cols)

}
