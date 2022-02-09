#' Gets colours for plotting snow density values
#'
#' Gets colours for plotting snow density values in snowprofiles. Colours are consistent with niViz at https://niviz.org
#'
#' @param Values Density values (kg/m3)
#' @param Resolution Resolution of colour scale. Default is 100.
#' @param Verbose Switch for writing out value and html colour tuplets for debugging.
#'
#' @return Array with HTML colour codes
#'
#' @author phaegeli
#'
#' @seealso [getColoursGrainSize], [getColoursGrainType], [getColoursHardness], [getColoursLWC], [getColoursSnowTemp]
#'
#' @examples
#'
#' Density <- seq(0,700, by=10)
#' plot(x = rep(1,length(Density)), y = Density, col = getColoursDensity(Density), pch = 19, cex = 3)
#'
#' @export
#'
getColoursDensity <- function(Values, Resolution = 101, Verbose = FALSE) {

  ## Base function
  getColourDensity <- function(Value, Resolution, Verbose) {

    ## Colours:
    ## 0: grey #F0F0F0
    ## 150: light blue #3CFBFB
    ## 300: medium blue #0084FF
    ## 450: blue #0000E1 >=
    ## 600: dark blue #00007F

    ClrPalette <- colorRampPalette(c("#F0F0F0", "#3CFBFB", "#0084FF", "#0000E1", "#00007F"))
    ClrRamp <- ClrPalette(Resolution)

    if (is.na(Value)) {
      Clr <- "#FFFFFF"
    } else if (Value < 0) {
      Clr <- "#FFFFFF"
    } else if (Value <= 600) {
      Clr <- ClrRamp[1 + round((Value/600) * (Resolution - 1), 0)]
    } else {
      Clr <- ClrRamp[Resolution]
    }

    if (Verbose) print(paste(Value, "-", Clr))

    return(Clr)
  }

  ## Application to array
  Clrs <- unlist(sapply(Values, getColourDensity, Resolution = Resolution, Verbose = Verbose))

  return(Clrs)
}
