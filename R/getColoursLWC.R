#' Gets colours for plotting LWC values
#'
#' Gets colours for plotting LWC values in snowprofiles. Colours are consistent with niViz at https://niviz.org
#'
#' @param Values Liquid water content values
#' @param Resolution Resolution of colour scale. Default is 100.
#' @param Verbose Switch for writing out value and html colour tuplets for debugging.
#'
#' @return Array with HTML colour codes
#'
#' @seealso [getColoursDensity], [getColoursGrainSize], [getColoursGrainType], [getColoursHardness], [getColoursSnowTemp]
#'
#' @author phaegeli
#'
#' @examples
#'
#' LWC <- seq(0,6, by = 0.1)
#' plot(x = rep(1,length(LWC)), y = LWC, col = getColoursLWC(LWC), pch = 19, cex = 3)
#'
#' @export
#'
getColoursLWC <- function(Values, Resolution = 101, Verbose = FALSE) {

  ## Base function
  getColourLWC <- function(Value, Resolution, Verbose) {

    ## Colours:
    ## 0: grey #F0F0F0
    ## 1: light blue #3CFBFB
    ## 2: medium blue #0084FF
    ## 3: blue #0000E1
    ## >= 4: dark blue #00007F

    ClrPalette <- colorRampPalette(c("#F0F0F0", "#3CFBFB", "#0084FF", "#0000E1", "#00007F"))
    ClrRamp <- ClrPalette(Resolution)

    if (is.na(Value)) {
      Clr <- "#FFFFFF"
    } else if (Value < 0) {
      Clr <- "#FFFFFF"
    } else if (Value <= 4) {
      Clr <- ClrRamp[1 + round((Value/4) * (Resolution - 1), 0)]
    } else {
      Clr <- ClrRamp[Resolution]
    }

    if (Verbose) print(paste(Value, "-", Clr))

    return(Clr)
  }

  ## Application to array
  Clrs <- unlist(sapply(Values, getColourLWC, Resolution = Resolution, Verbose = Verbose))

  return(Clrs)
}
