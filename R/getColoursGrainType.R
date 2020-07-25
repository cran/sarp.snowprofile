#' Gets colours for plotting snow grain types
#'
#' Grain colours are defined in the `grainDict` data.frame and the definitions can be changed with `setColoursGrainType`
#'
#' @param Grains grain type (character or list of characters)
#' @param grainDict. lookup table to use. Note, the easiest and best way to do this is via `setColoursGrainType`. This input variable here
#' is only a hack to change the grainDict explicitly when calling `plot.snowprofile` via `Col`, and beforehand computing
#' `Col = Col <- sapply(Profile$layers$gtype, function(x) getColoursGrainType(x, grainDict = setColoursGrainType('sarp-reduced')))`;
#' This is only necessary in specific environments (e.g. a shiny app)
#'
#' @return Array with HTML colour codes
#'
#' @author phaegeli, shorton, fherla
#'
#' @seealso [setColoursGrainType], [getColoursDensity], [getColoursGrainSize], [getColoursHardness], [getColoursLWC], [getColoursSnowTemp]
#'
#' @examples
#'
#' Grains <- c('PP', 'DF', 'RG', 'FC', 'FCxr', 'DH', 'SH', 'MF', 'MFcr', 'IF')
#' Colours <- getColoursGrainType(Grains)
#' Colours
#'
#' plot(1:length(Grains), col = Colours, pch = 20, cex = 3)
#' text(1:length(Grains), 1:length(Grains), Grains, pos = 1)
#'
#' @export
#'
getColoursGrainType <- function(Grains, grainDict. = grainDict) {
  Col <- sapply(Grains, function(x) grainDict$colour[grainDict.$gtype == x][1])
  Col[is.na(Col)] <- "gray"
  return(Col)
}
