#' Compute Relative Threshold Sum approach (RTA)
#'
#' @param profile a [snowprofile] object, whose layer properties are entirely known (i.e., no NAs in gtype, hardness, gsize allowed!)
#' @param target Do you want to compute the index for the layers or for the layer interfaces? defaults to both.
#'
#' @return the snowprofile object is returned with new layer properties describing the index. The index ranges between `[0, 1]`, whereas values > 0.8
#' indicate structurally unstable layers/interfaces.
#'
#' @seealso [computeTSA]
#' @description Monti, F., & Schweizer, J. (2013). A relative difference approach to detect potential weak layers within a snow profile.
#' Proceedings of the 2013 International Snow Science Workshop, Grenoble, France, 339–343. Retrieved from https://arc.lib.montana.edu/snow-science/item.php?id=1861
#'
#' @author fherla
#'
#' @examples
#' sp <- computeRTA(SPpairs$B_modeled1)
#' plot(sp, TempProfile = FALSE, main = "RTA")
#' lines(sp$layers$rta*5, sp$layers$height - 0.5*sp$layers$thickness, type = "b", xlim = c(0, 5))
#' lines(sp$layers$rta_interface*5, sp$layers$height, type = "b", xlim = c(0, 5), col = "red")
#' abline(h = sp$layers$height, lty = "dotted", col = "grey")
#' abline(v = 0.8*5, lty = "dashed")
#'
#' @export
computeRTA <- function(profile, target = c("interface", "layer")) {

  if (!is.snowprofile(profile)) stop("profile is not a snowprofile object")
  lyrs <- profile$layers
  if (!"gsize" %in% names(lyrs)) {
    if ("gsize_avg" %in% names(lyrs)) lyrs$gsize <- lyrs$gsize_avg
    if ("gsize_max" %in% names(lyrs)) lyrs$gsize <- lyrs$gsize_max
    if (!"gsize" %in% names(lyrs)) stop("No gsize available in profile!")
  }
  if (!all(c("hardness", "gtype", "depth") %in% names(lyrs))) stop("Critical layer property is missing for RTA computation!")

  nL <- nrow(lyrs)
  if (nL < 2) {
    lyrs$rta <- 0
    lyrs$rta_interface <- 0
  } else {

    ## two absolute thresholds also remain in the relative index
    thresh_gtype <- c("FC", "FCxr", "DH", "SH")
    thresh_depth <- 100

    ## compute interface properties
    dhardness_upper <- abs(diff(c(NA, lyrs$hardness)))
    dhardness_lower <- abs(diff(c(lyrs$hardness, NA)))

    dgsize_upper <- abs(diff(c(NA, lyrs$gsize)))
    dgsize_lower <- abs(diff(c(lyrs$gsize, NA)))

    ## compute average quantities
    hbar <- mean(lyrs$hardness)
    dhbar <- mean(dhardness_lower, na.rm = TRUE)
    gbar <- mean(lyrs$gsize)
    dgbar <- mean(dgsize_lower, na.rm = TRUE)

    if (any(is.na(lyrs$hardness)) | sd_sample_uncorrected(lyrs$hardness, hbar) == 0) warning("All hardness values identical, or one layer NA --> RTA = NA")
    if (any(is.na(lyrs$gsize)) | sd_sample_uncorrected(lyrs$gsize, gbar) == 0) warning("All gsize values identical, or one layer NA --> RTA = NA")

    ## compute relative quantities  (this procedure already corresponds to step 1 of layer-wise RTA computation: upper interface)
    bolmat <- matrix(nrow = nrow(lyrs), ncol = 6)
    colnames(bolmat) <- c("gtype", "hardness", "gsize", "dhardness", "dgsize", "depth")
    bolmat[, "gtype"] <- lyrs$gtype %in% thresh_gtype
    bolmat[, "hardness"] <- (lyrs$hardness - hbar) / sd_sample_uncorrected(lyrs$hardness, hbar)
    bolmat[, "gsize"] <- (lyrs$gsize - gbar) / sd_sample_uncorrected(lyrs$gsize, gbar)
    bolmat[, "dhardness"] <- (dhardness_upper - dhbar) / sd_sample_uncorrected(na.omit(dhardness_upper), dhbar)
    bolmat[, "dgsize"] <- (dgsize_upper - dgbar) / sd_sample_uncorrected(na.omit(dgsize_upper), dgbar)
    bolmat[, "depth"] <- lyrs$depth < thresh_depth

    ## compute min and max of relative quantities
    minmax_h <- c(min(bolmat[, "hardness"]), max(bolmat[, "hardness"]))
    minmax_dh <- c(min(bolmat[, "dhardness"], na.rm = TRUE), max(bolmat[, "dhardness"], na.rm = TRUE))
    minmax_g <- c(min(bolmat[, "gsize"]), max(bolmat[, "gsize"]))
    minmax_dg <- c(min(bolmat[, "dgsize"], na.rm = TRUE), max(bolmat[, "dgsize"], na.rm = TRUE))

    if ("layer" %in% target) {
      ## scale relative quantities to [0, 1]
      bolmat[, "hardness"] <- (bolmat[, "hardness"] - minmax_h[1]) / (minmax_h[2] - minmax_h[1])
      bolmat[, "gsize"] <- (bolmat[, "gsize"] - minmax_g[1]) / (minmax_g[2] - minmax_g[1])
      bolmat[, "dhardness"] <- (bolmat[, "dhardness"] - minmax_dh[1]) / (minmax_dh[2] - minmax_dh[1])
      bolmat[, "dgsize"] <- (bolmat[, "dgsize"] - minmax_dg[1]) / (minmax_dg[2] - minmax_dg[1])

      ## compute relative quantities part II: lower interface
      bolmat_below <- bolmat
      bolmat_below[, "dhardness"] <- (dhardness_lower - dhbar) / sd_sample_uncorrected(na.omit(dhardness_lower), dhbar)
      bolmat_below[, "dgsize"] <- (dgsize_lower - dgbar) / sd_sample_uncorrected(na.omit(dgsize_lower), dgbar)
      ## scale relative quantities to [0, 1]: part II
      bolmat_below[, "dhardness"] <- (bolmat_below[, "dhardness"] - minmax_dh[1]) / (minmax_dh[2] - minmax_dh[1])
      bolmat_below[, "dgsize"] <- (bolmat_below[, "dgsize"] - minmax_dg[1]) / (minmax_dg[2] - minmax_dg[1])

      lyrs$rta <- pmax(rowSums(bolmat, na.rm = FALSE), rowSums(bolmat_below, na.rm = FALSE), na.rm = TRUE) / 6
    }

    if ("interface" %in% target) {
      ## compute relative quantities
      ## i) using layer properties from layer above
      bolmat_above <- matrix(nrow = nrow(lyrs), ncol = 6)
      colnames(bolmat_above) <- c("gtype", "hardness", "gsize", "dhardness", "dgsize", "depth")
      bolmat_above[, "gtype"] <- c(lyrs$gtype[-1], NA) %in% thresh_gtype
      bolmat_above[, "hardness"] <- (c(lyrs$hardness[-1], NA) - hbar) / sd_sample_uncorrected(c(lyrs$hardness[-1]), hbar)
      bolmat_above[, "gsize"] <- (c(lyrs$gsize[-1], NA) - gbar) / sd_sample_uncorrected(c(lyrs$gsize[-1]), gbar)
      bolmat_above[, "dhardness"] <- (dhardness_lower - dhbar) / sd_sample_uncorrected(na.omit(dhardness_lower), dhbar)
      bolmat_above[, "dgsize"] <- (dgsize_lower - dgbar) / sd_sample_uncorrected(na.omit(dgsize_lower), dgbar)
      bolmat_above[, "depth"] <- c(lyrs$depth[-1], NA) < thresh_depth

      ## ii) using layer properties from layer below
      bolmat_below[, "gtype"] <- lyrs$gtype %in% thresh_gtype
      bolmat_below[, "hardness"] <- (lyrs$hardness - hbar) / sd_sample_uncorrected(lyrs$hardness, hbar)
      bolmat_below[, "gsize"] <- (lyrs$gsize - gbar) / sd_sample_uncorrected(lyrs$gsize, gbar)
      bolmat_below[, "dhardness"] <- (dhardness_lower - dhbar) / sd_sample_uncorrected(na.omit(dhardness_lower), dhbar)
      bolmat_below[, "dgsize"] <- (dgsize_lower - dgbar) / sd_sample_uncorrected(na.omit(dgsize_lower), dgbar)
      bolmat_below[, "depth"] <- lyrs$depth < thresh_depth

      ## scale relative quantities to [0, 1]
      bolmat_above[, "hardness"] <- (bolmat_above[, "hardness"] - minmax_h[1]) / (minmax_h[2] - minmax_h[1])
      bolmat_above[, "gsize"] <- (bolmat_above[, "gsize"] - minmax_g[1]) / (minmax_g[2] - minmax_g[1])
      bolmat_above[, "dhardness"] <- (bolmat_above[, "dhardness"] - minmax_dh[1]) / (minmax_dh[2] - minmax_dh[1])
      bolmat_above[, "dgsize"] <- (bolmat_above[, "dgsize"] - minmax_dg[1]) / (minmax_dg[2] - minmax_dg[1])

      bolmat_below[, "hardness"] <- (bolmat_below[, "hardness"] - minmax_h[1]) / (minmax_h[2] - minmax_h[1])
      bolmat_below[, "gsize"] <- (bolmat_below[, "gsize"] - minmax_g[1]) / (minmax_g[2] - minmax_g[1])
      bolmat_below[, "dhardness"] <- (bolmat_below[, "dhardness"] - minmax_dh[1]) / (minmax_dh[2] - minmax_dh[1])
      bolmat_below[, "dgsize"] <- (bolmat_below[, "dgsize"] - minmax_dg[1]) / (minmax_dg[2] - minmax_dg[1])


      lyrs$rta_interface <- pmax(rowSums(bolmat_above, na.rm = FALSE), rowSums(bolmat_below, na.rm = FALSE), na.rm = FALSE) / 6
    }
  }

  profile$layers <- lyrs
  return(profile)


}



