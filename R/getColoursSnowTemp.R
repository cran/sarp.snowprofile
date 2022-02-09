#' Gets colours for plotting snow temperature values
#'
#' Gets colours for plotting snow temperature values in snowprofiles. Colours are consistent with niViz at https://niviz.org
#'
#' @param Values Snow temperature values
#' @param Resolution Resolution of colour scale. Default is 100.
#' @param Verbose Switch for writing out value and html colour tuplets for debugging.
#'
#' @return Array with HTML colour codes
#'
#' @seealso [getColoursDensity], [getColoursGrainSize], [getColoursGrainType], [getColoursHardness], [getColoursLWC]
#'
#' @author phaegeli
#'
#' @examples
#'
#' SnowTemp <- c(-25:0)
#' plot(x = rep(1,length(SnowTemp)), y = SnowTemp,
#'      col = getColoursSnowTemp(SnowTemp), pch = 19,cex = 3)
#'
#' @export
#'
getColoursSnowTemp <- function(Values, Resolution = 101, Verbose = FALSE) {

  ## Base function
  getColourSnowTemp <- function(Value, Resolution, Verbose = FALSE) {

    ## Colours:
    ## >= -20: dark blue #00007F
    ## -15: blue #0000E1
    ## -10: light blue #3CFBFB
    ## -5: grey #F0F0F0
    ## 0: red #FF0000

    ClrPalette <- colorRampPalette(c("#00007F", "#0000E1", "#3CFBFB", "#F0F0F0", "#FF0000"))
    ClrRamp <- ClrPalette(Resolution)

    if (is.na(Value)) {
      Clr <- "#FFFFFF"
    } else if (Value > 0) {
      Clr <- "#FFFFFF"
    } else if (Value < -20) {
      Clr <- ClrRamp[1]
    } else {
      Clr <- ClrRamp[1 + round((Value + 20)/20 * (Resolution - 1), 0)]
    }

    if (Verbose) print(paste(Value, "-", Clr))

    return(Clr)
  }

  ## Application to array
  Clrs <- unlist(sapply(Values, getColourSnowTemp, Resolution = Resolution, Verbose = Verbose))

  return(Clrs)

}
