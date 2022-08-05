#' Compute 'density over grain size' averaged over slab
#'
#' For each layer, compute the average density over grain size of all layers above, i.e. `<rho/gs>_slab`.
#' This variable has been found to characterize the cohesion of slabs: new snow slabs tend to consist of low density & large grains, and
#' more cohesive slabs of older snow tend to consist of higher density & smaller grains (Mayer et al, 2022 in review).
#'
#' @param profile [snowprofile] object
#' @param implementation `"pub"` for `<rho/gs>_slab`, `"literal"` for 'mean density of slab over mean grain size of slab' `<rho>_slab / <gs>_slab`.
#'
#' @return snowprofile object with added layers column `$slab_rhogs`. Note that topmost layer is always `NA`.
#'
#' @author fherla
#' @export
computeSLABrhogs <- function(profile, implementation = c("pub", "literal")[1]) {

  if (!is.snowprofile(profile)) stop("profile is not a snowprofile object")
  lyrs <- profile$layers
  nL <- nrow(lyrs)
  if (!"gsize" %in% names(lyrs)) {
    if ("gsize_avg" %in% names(lyrs)) lyrs$gsize <- lyrs$gsize_avg
    if ("gsize_max" %in% names(lyrs)) lyrs$gsize <- lyrs$gsize_max
    if (!"gsize" %in% names(lyrs)) stop("No gsize available in profile!")
  }
  if (!"density" %in% names(lyrs)) stop("Density is missing for slab_rhogs computation!")

  if (nL < 2) {
    lyrs$slab_rhogs <- NA
  } else {
    if (implementation == "literal") {
      ## literal interpretation of mean density of slab over mean grain size of slab: <rho>_{slab} / <gs>_{slab}
      slab_rhobar <- c(rev(cumsum(rev(lyrs$density * lyrs$thickness)) / cumsum(rev(lyrs$thickness)))[2:nL], NA)
      slab_gsbar <- c(rev(cumsum(rev(lyrs$gsize * lyrs$thickness)) / cumsum(rev(lyrs$thickness)))[2:nL], NA)
      lyrs$slab_rhogs <- slab_rhobar/slab_gsbar

    } else {
      ## Stephie's implementation: <rho/gs>_{slab}
      rhogs <- lyrs$density * lyrs$thickness / lyrs$gsize
      lyrs$slab_rhogs <- round(c(rev(cumsum(rev(rhogs)) / cumsum(rev(lyrs$thickness)))[2:nL], NA))
      ## identical but way slower:
      # slab_rhogs2 <- c(sapply(seq(nL-1), function(i) sum(rhogs[(i+1):nL]) / sum(lyrs$thickness[(i+1):nL])), NA)
    }
  }

  profile$layers <- lyrs
  return(profile)
}


#' Compute mean density of slab
#'
#' For each layer, compute the average density of all layers above, i.e. `<rho>_slab`.
#'
#' @param profile [snowprofile] object
#'
#' @return snowprofile object with added layers column `$slab_rho`. Note that topmost layer is always `NA`.
#'
#' @author fherla
#' @export
computeSLABrho <- function(profile) {

  if (!is.snowprofile(profile)) stop("profile is not a snowprofile object")
  lyrs <- profile$layers
  nL <- nrow(lyrs)
  if (!"density" %in% names(lyrs)) stop("Density is missing for slab_rho computation!")

  if (nL < 2) {
    lyrs$slab_rho <- NA
  } else {
    rho <- lyrs$density * lyrs$thickness
    lyrs$slab_rho <- round(c(rev(cumsum(rev(rho)) / cumsum(rev(lyrs$thickness)))[2:nL], NA))
  }

  profile$layers <- lyrs
  return(profile)
}
