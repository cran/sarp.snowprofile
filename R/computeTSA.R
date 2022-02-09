#' Compute Threshold Sum Approach (TSA, lemons, yellow flags, 'Nieten')
#'
#' This routine computes the traditional lemons (german 'Nieten') based on absolute thresholds. Since the thresholds are
#' defined in Monti (2014) with different thresholds for manual versus observed profiles, this routine switches between the appropriate
#' thresholds based on the `$type` field of the input profile. While `manual` and `whiteboard` profiles get one set of thresholds,
#' `modeled`, `vstation`, and `aggregate` type profiles get another set.
#'
#' @param profile a [snowprofile] object
#' @param target Do you want to compute the index for the layers or for the layer interfaces? defaults to both.
#'
#' @return the snowprofile object is returned with new layer properties describing the threshold sums. The TSA sums up to 6 indicators,
#' whereas >= 5 indicators indicate structurally unstable layers/interfaces.
#'
#' @seealso [computeRTA]
#' @references
#' Schweizer, J., & Jamieson, J. B. (2007). A threshold sum approach to stability evaluation of manual snow profiles.
#' Cold Regions Science and Technology, 47(1–2), 50–59. https://doi.org/10.1016/j.coldregions.2006.08.011
#'
#' Monti, F., Schweizer, J., & Fierz, C. (2014). Hardness estimation and weak layer detection in simulated snow stratigraphy.
#' Cold Regions Science and Technology, 103, 82–90. https://doi.org/10.1016/j.coldregions.2014.03.009
#'
#' @author fherla
#' @examples
#' sp <- computeTSA(SPpairs$B_modeled1)
#' plot(sp, TempProfile = FALSE, main = "TSA")
#' lines(sp$layers$tsa/6*5,
#'       sp$layers$height - 0.5*sp$layers$thickness, type = "b", xlim = c(0, 5))
#' lines(sp$layers$tsa_interface/6*5, sp$layers$height, type = "b", xlim = c(0, 5), col = "red")
#' abline(h = sp$layers$height, lty = "dotted", col = "grey")
#' abline(v = 5/6*5, lty = "dashed")
#'
#' @export
computeTSA <- function(profile, target = c("interface", "layer")) {

  if (!is.snowprofile(profile)) stop("profile is not a snowprofile object")
  ## --- Initialization ----
  lyrs <- profile$layers
  if (!"gsize" %in% names(lyrs)) {
    if ("gsize_avg" %in% names(lyrs)) lyrs$gsize <- lyrs$gsize_avg
    if ("gsize_max" %in% names(lyrs)) lyrs$gsize <- lyrs$gsize_max
    if (!"gsize" %in% names(lyrs)) stop("No gsize available in profile!")
  }
  nL <- nrow(lyrs)
  if (nL < 2) {
    lyrs$tsa <- 0
    lyrs$tsa_interface <- 0
  } else {

    ## define thresholds
    if (profile$type %in% c("manual", "whiteboard")) {
      thresh_gtype <- c("FC", "FCxr", "DH", "SH")
      thresh_hardness <- 1.3
      thresh_gsize <- 1.25
      thresh_dhardness <- 1.7
      thresh_dgsize <- 0.75
      thresh_dgsize_unit <- "abs"
      thresh_depth <- 100
    } else {
      thresh_gtype <- c("FC", "FCxr", "DH", "SH")
      thresh_hardness <- 2
      thresh_gsize <- 0.6
      thresh_dhardness <- 1
      thresh_dgsize <- 0.4
      thresh_dgsize_unit <- "rel"
      thresh_depth <- 100
    }

    ## compute interface properties
    dhardness_upper <- abs(diff(c(NA, lyrs$hardness)))
    dhardness_lower <- abs(diff(c(lyrs$hardness, NA)))

    if (thresh_dgsize_unit == "abs") {
      dgsize_upper <- abs(diff(c(NA, lyrs$gsize)))
      dgsize_lower <- abs(diff(c(lyrs$gsize, NA)))
    } else {
      dgsize_upper <- pmax(abs((lyrs$gsize / shift(lyrs$gsize)) - 1), abs((shift(lyrs$gsize) / lyrs$gsize) - 1))
      dgsize_lower <- pmax(abs((lyrs$gsize / shift(lyrs$gsize, type = "lead")) - 1), abs((shift(lyrs$gsize, type = "lead") / lyrs$gsize) - 1))
    }

    ## --- TSA for each layer ----
    if ("layer" %in% target) {
      ## TSA with upper interface
      bolmat <- matrix(nrow = nrow(lyrs), ncol = 6)
      colnames(bolmat) <- c("gtype", "hardness", "gsize", "dhardness", "dgsize", "depth")
      bolmat[, "gtype"] <- lyrs$gtype %in% thresh_gtype
      bolmat[, "hardness"] <- lyrs$hardness <= thresh_hardness
      bolmat[, "gsize"] <- lyrs$gsize >= thresh_gsize
      bolmat[, "dhardness"] <- dhardness_upper >= thresh_dhardness
      bolmat[, "dgsize"] <- dgsize_upper >= thresh_dgsize
      bolmat[, "depth"] <- lyrs$depth < thresh_depth

      ## TSA with lower interface
      bolmat_below <- bolmat
      bolmat_below[, "dhardness"] <- dhardness_lower >= thresh_dhardness
      bolmat_below[, "dgsize"] <- dgsize_lower >= thresh_dhardness

      lyrs$tsa <- pmax(rowSums(bolmat, na.rm = FALSE), rowSums(bolmat_below, na.rm = FALSE), na.rm = TRUE)
    }

    ## --- TSA for each interface ----
    ## i.e., same nrows in data.frame,
    ## first row corresponds to top interface of bottom layer (i.e., No TSA for interface to ground!),
    ## last row corresponds to snow surface and needs to be NA.
    if ("interface" %in% target) {
      ## i) using layer properties from layer above
      bolmat_above <- matrix(nrow = nrow(lyrs), ncol = 6)
      colnames(bolmat_above) <- c("gtype", "hardness", "gsize", "dhardness", "dgsize", "depth")
      bolmat_above[, "gtype"] <- c(lyrs$gtype[-1], NA) %in% thresh_gtype
      bolmat_above[, "hardness"] <- c(lyrs$hardness[-1], NA) <= thresh_hardness
      bolmat_above[, "gsize"] <- c(lyrs$gsize[-1], NA) >= thresh_gsize
      bolmat_above[, "dhardness"] <- dhardness_lower >= thresh_dhardness
      bolmat_above[, "dgsize"] <- dgsize_lower >= thresh_dgsize
      bolmat_above[, "depth"] <- c(lyrs$depth[-1], NA) < thresh_depth

      ## ii) using layer properties from layer below
      bolmat_below <- matrix(nrow = nrow(lyrs), ncol = 6)
      colnames(bolmat_below) <- c("gtype", "hardness", "gsize", "dhardness", "dgsize", "depth")
      bolmat_below[, "gtype"] <- lyrs$gtype %in% thresh_gtype
      bolmat_below[, "hardness"] <- lyrs$hardness <= thresh_hardness
      bolmat_below[, "gsize"] <- lyrs$gsize >= thresh_gsize
      bolmat_below[, "dhardness"] <- dhardness_lower >= thresh_dhardness
      bolmat_below[, "dgsize"] <- dgsize_lower >= thresh_dgsize
      bolmat_below[, "depth"] <- lyrs$depth < thresh_depth

      lyrs$tsa_interface <- pmax(rowSums(bolmat_above, na.rm = FALSE), rowSums(bolmat_below, na.rm = FALSE))
    }
  }

  profile$layers <- lyrs
  return(profile)

}
