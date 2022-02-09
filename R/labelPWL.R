#' @describeIn findPWL Label layers of interest (e.g., weak layers) in `snowprofile`
#' @param ... passed on to [findPWL]
#' @return `labelPWL`: The input object with an extra boolean column appended to the layer object, called `$layerOfInterest`.
#' @details The `labelPWL` wrapper function is primarily used by `sarp.snowprofile.alignment::averageSP`.
#' @export
labelPWL <- function(x, ...) {

  ## Assertions
  if (!is.snowprofile(x)) stop("In labelPWL, x needs to be a snowprofile object")

  ## Call to findPWL
  idx <- findPWL(x, ...)

  ## Label layers
  x$layers$layerOfInterest <- FALSE
  x$layers$layerOfInterest[idx] <- TRUE

  return(x)

}
