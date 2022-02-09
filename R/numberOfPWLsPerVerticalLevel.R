#' Count number of PWLs per vertical level
#'
#' This is a wrapper function to bin several weak layers (or crusts) into vertical levels.
#' The layers to be binned can be controled with a provided index vector for full customization.
#'
#' @param x [snowprofile] or [snowprofileLayers] object
#' @param pwl_idx an index vector that corresponds to the layers of interest. Tip: this can also be a call to [findPWL], see examples.
#' @param depth_breaks a vector of break points referring to absolute depth values. `Inf` is a placeholder for max depth.
#'
#' @return This function returns a `table` object
#'
#' @author fherla
#'
#' @examples
#' SH_idx <- findPWL(SPpairs$C_day1, pwl_gtype = "SH")
#' numberOfPWLsPerVerticalLevel(SPpairs$C_day1, SH_idx)
#'
#' numberOfPWLsPerVerticalLevel(SPpairs$C_day2, findPWL(SPpairs$C_day2))
#'
#' @export
numberOfPWLsPerVerticalLevel <- function(x, pwl_idx, depth_breaks = c(0, 30, 80, 150, Inf)) {

  if (is.snowprofile(x)) {
    layers <- x$layers
  } else if (is.snowprofileLayers(x)) {
    layers <- x
  } else {
    stop("x needs to be a snowprofile(Layers) object")
  }

  if (!"depth" %in% names(layers)) stop("'depth' column missing in layers object")

  depth_breaks[is.infinite(depth_breaks)] <- max(layers$depth)
  depth_breaks <- unique(depth_breaks[depth_breaks <= max(layers$depth)])
  if (length(depth_breaks) < 2) {
    res <- as.table(length(pwl_idx))
    names(res) <- "[0, 0)"
  } else {
    res <- table(cut(layers$depth[pwl_idx], depth_breaks, include.lowest = TRUE))
  }

  return(res)
}
