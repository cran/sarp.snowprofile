#' Conversion of character Aspects to numeric Aspects
#'
#' Convert character aspects (of snow profile locations) to numeric values.
#' For example, Aspect "N" (north) becomes 0 degrees azimuth.
#'
#' @param charAspect Character string of aspect location, i.e., one of
#'   - `c("N", "NE", "NNE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW")`
#'
#' @return Float value of numeric aspect location, North = 0 degree, S = 180 degree
#'
#' @author fherla
#'
#' @examples
#' char2numAspect("W")
#' char2numAspect("WNW")
#'
#' char2numAspect(c("N", NA, "NA", "NE"))
#'
#' @export
#'
char2numAspect <- function(charAspect) {

  charAspect[is.na(charAspect)] <- "NA"
  if (!all(is.character(charAspect))) stop("Not all elements are of type character")

  ## Assign numeric values to each element of hand hardness index
  characterAspect <- c("NA", "N", "NE", "NNE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW")
  numericAspect <- c(NA, 0, 45, 22.5, 67.5, 90, 112.5, 135, 157.5, 180, 202.5, 225, 247.5, 270, 292.5, 315)
  transMat <- data.frame(numericAspect = numericAspect, row.names = characterAspect)

  numAspect <- transMat[charAspect,]

  return(numAspect)
}
