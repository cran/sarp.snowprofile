#' Gets colours for plotting snow hardness values
#'
#' Gets colours for plotting snow hardness values in snowprofiles.
#'
#' @param Values Hardness values
#' @param Resolution Resolution of colour scale. Default is 100.
#' @param Verbose Switch for writing out value and html colour tuplets for debugging.
#'
#' @return Array with HTML colour codes
#'
#' @seealso [getColoursDensity], [getColoursGrainSize], [getColoursGrainType], [getColoursLWC], [getColoursSnowTemp]
#'
#' @author phaegeli
#'
#' @examples
#'
#' Hardness <- c(1:5)
#' plot(x = rep(1,length(Hardness)), y = Hardness,
#'      col = getColoursHardness(Hardness), pch = 19,cex = 3)
#'
#' @export
#'
getColoursHardness <- function(Values, Resolution = 101, Verbose = FALSE) {

  ## Base function
  getColourHardness <- function(Value, Resolution, Verbose = FALSE) {

    ## Colours:
    ## Colour brewer puples (5 levels)
    ## 1: #f2f0f7
    ## 2: #cbc9e2
    ## 3: #9e9ac8
    ## 4: #756bb1
    ## 5: #54278f

    ClrPalette <- colorRampPalette(c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#756bb1", "#54278f"))
    ClrRamp <- ClrPalette(Resolution)

    if (is.na(Value)) {
      Clr <- "#FFFFFF"
    } else if (Value > 5 | Value < 1) {
      Clr <- "#FFFFFF"
    } else {
      Clr <- ClrRamp[1 + round((Value - 1)/4 * (Resolution - 1), 0)]
    }

    if (Verbose) print(paste(Value, "-", Clr))

    return(Clr)
  }

  ## Application to array
  Clrs <- unlist(sapply(Values, getColourHardness, Resolution = Resolution, Verbose = Verbose))

  return(Clrs)
}
