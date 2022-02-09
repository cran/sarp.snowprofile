#' Derive datetag from deposition dates in simulated profiles
#'
#' This routine derives the datetags of simulated snow profile layers from deposition dates. Datetags usually are deposition dates
#' for crust layers, and burial dates for other weak layers (e.g., SH, FC). If no datetags can be derived, a datetag column of NAs will
#' nevertheless be added to the snowprofile layers. The routine also adds a `bdate` column for burial dates that are calculated along the way.
#'
#' @param x a [snowprofile] or [snowprofileLayers] object
#' @return The `x` object will be returned with added columns `datetag` and `bdate` (i.e., either as `snowprofile`, or `snowprofileLayers`)
#' @author fherla
#'
#' @examples
#' ## This is not the most meaningful example, but it nicely illustrates the routine:
#' print(SPpairs$A_manual)
#'
#' print(deriveDatetag(SPpairs$A_manual))
#'
#' @export
#'
deriveDatetag <- function(x) {

  ## --- Assertions / Initializations ----
  if (is.snowprofile(x)) {
    orig_layers <- x$layers
  } else if (is.snowprofileLayers(x)) {
    orig_layers <- x
  } else {
    stop("x needs to be a snowprofile or snowprofileLayers object")
  }

  ## add datetag column with NAs
  nL <- nrow(orig_layers)
  layers <- orig_layers
  if (!"bdate" %in% names(layers)) layers <- cbind(layers, data.frame(bdate = rep(as.POSIXct(NA), times = nL)))
  if (!"datetag" %in% names(layers)) layers <- cbind(layers, data.frame(datetag = rep(as.POSIXct(NA), times = nL)))

  ## ---Calculations----
  if ("ddate" %in% names(layers) & nL > 1) {

    ## don't create bdate for unobserved basal layer:
    lag <- 0
    if (hasUnobservedBasalLayer(orig_layers)) lag <- 1

    ## derive bdate from ddate:
    layers[(1+lag):(nL-1), "bdate"] <- layers[(2+lag):nL, "ddate"]
    ## check
    if (any(layers[1:(nL-1), "bdate"] < layers[1:(nL-1), "ddate"], na.rm = TRUE)) stop("Layer order incorrect, layers get buried before they form!")

    ## merge ddate and bdate into datetag:
    layers[, "datetag"] <- layers[, "bdate"]
    layers[layers[, "gtype"] %in% c("MFcr", "IF"), "datetag"] <- layers[layers[, "gtype"] %in% c("MFcr", "IF"), "ddate"]

  } else if (!"ddate" %in% names(layers)) {
    warning("No ddate info available, returning NA datetags")
  }

  class(layers) <- class(orig_layers)
  if (is.snowprofile(x)) {
    x$layers <- layers
    return(x)
  } else if (is.snowprofileLayers(x)) {
    return(layers)
  }
}
