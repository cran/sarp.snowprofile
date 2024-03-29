#' Compute Relative Threshold Sum approach (RTA)
#'
#' @description
#' The function can compute the RTA index for layers and for interfaces. The calculation follows the example in Monti (2013),
#' referenced below. The six individual relative lemons are computed as follows. To compute the RTA index for layers,
#' the layer properties are combined with the interface properties of the weakest interface below or above the layer.
#' To compute the RTA index for interfaces, the interface properties are combined with the weakest layer properties below
#' or above the interface. The six properties considered in the index are
#'
#'   * grain size, hardness, grain type (layer properties)
#'   * difference of grain sizes and hardness (at the interface)
#'   * depth (at the top interface of the layer)
#'
#' Instead of implementing a static threshold for the depth weighting, the depth is scaled with a weibull function
#' that is corrected for potential crusts and their stabilizing effects (Monti and Mitterer, personal communication).
#'
#' Note that due to the crust correction, the results from this function will only be correct if applied to profiles
#' that have not yet been resampled (such as by functions from sarp.snowprofile.alignment: `resampleSP`, `resampleSPpairs`,
#' `dtw`, `averageSP`).
#'
#' The RTA index ranges between `[0, 1]`, with the weakest layer/interface euqal to 1. Values > 0.8 indicate layers/interfaces with a poor structural stability.
#'
#' @param x a [snowprofile] or [snowprofileSet]. Profile layer properties must be known for all layers (i.e., no NAs in gtype, hardness, gsize allowed!)
#' @param target Do you want to compute the index for the layers or for the layer interfaces? defaults to both.
#'
#' @return The input object will be returned with the new layer properties `rta`/`rta_interface` describing the RTA index added to the profile layers.
#'
#'
#' @seealso [computeTSA]
#' @references Monti, F., & Schweizer, J. (2013). A relative difference approach to detect potential weak layers within a snow profile.
#' Proceedings of the 2013 International Snow Science Workshop, Grenoble, France, 339–343. Retrieved from https://arc.lib.montana.edu/snow-science/item.php?id=1861
#'
#'
#'
#' @author fherla
#'
#' @export
computeRTA <- function(x, target = c("interface", "layer")) UseMethod("computeRTA")

#' @describeIn computeRTA for [snowprofileSet]s
#' @export
#' @examples
#' ## apply function to snowprofileSet
#' profileset <- computeRTA(SPgroup)
#'
computeRTA.snowprofileSet <- function(x, target = c("interface", "layer")) {
  if (!is.snowprofileSet(x)) stop("x is not a snowprofileSet")
  x <- snowprofileSet(lapply(x, computeRTA.snowprofile, target = target))
  return(x)
}

#' @describeIn computeRTA for [snowprofile]s
#' @examples
#' ## apply function to snowprofile and plot output
#' sp <- computeRTA(SPpairs$B_modeled1)
#' plot(sp, TempProfile = FALSE, main = "RTA")
#' lines(sp$layers$rta*5, sp$layers$height - 0.5*sp$layers$thickness, type = "b", xlim = c(0, 5))
#' lines(sp$layers$rta_interface*5, sp$layers$height, type = "b", xlim = c(0, 5), col = "red")
#' abline(h = sp$layers$height, lty = "dotted", col = "grey")
#' abline(v = 0.8*5, lty = "dashed")
#' @export
computeRTA.snowprofile <- function(x, target = c("interface", "layer")) {

  if (!is.snowprofile(x)) stop("profile is not a snowprofile object")
  lyrs <- x$layers
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

    if (nL > 3) {
      if (all(diff(lyrs$height[2:nL]) < 1)) warning("It looks like you're computing RTA for a resampled profile. Please read the function documentation about errors introduced in that case!")
    }

    ## one absolute threshold remains in the relative index
    thresh_gtype <- c("FC", "FCxr", "DH", "SH")
    # thresh_depth <- 100  # SNOWPACK implementation uses a weibull distribution for depth weighting
                           # instead of a fixed threshold of 100 cm! Same approach has been implemented below, too.

    ## compute interface properties
    dhardness_upper <- abs(diff(c(NA, lyrs$hardness)))
    dhardness_lower <- abs(diff(c(lyrs$hardness, NA)))

    dgsize_upper <- abs(diff(c(NA, lyrs$gsize)))
    dgsize_lower <- abs(diff(c(lyrs$gsize, NA)))

    ## compute average layer quantities
    hbar <- mean(lyrs$hardness)
    dhbar <- mean(dhardness_lower, na.rm = TRUE)
    gbar <- mean(lyrs$gsize)
    dgbar <- mean(dgsize_lower, na.rm = TRUE)

    ## weibull depth weighting
    w1 <- 2.5
    w2 <- 50
    weibull_depth <- (w1/w2) * lyrs$depth**(w1-1) * exp(-1*(lyrs$depth/w2)**w1)
    crust_value <- rep(0, times = nrow(lyrs))
    crust_value[lyrs$hardness >= 3 & lyrs$thickness > 1] <- exp(-lyrs$depth[lyrs$hardness >= 3 & lyrs$thickness > 1]/20)
    crust_coeff <- rev(cumsum(rev(crust_value)))  # the double 'rev' command computes the cumulative sum from the snow surface towards the bottom!
    weibull_adjusted<- weibull_depth - crust_coeff
    weibullbar <- mean(weibull_adjusted)

    if (any(is.na(lyrs$hardness))) warning("All hardness values identical, or one layer NA --> RTA = NA")
    if (any(is.na(lyrs$gsize)) | sd_sample_uncorrected(lyrs$gsize, gbar) == 0) warning("All gsize values identical, or one layer NA --> RTA = NA")

    ## note the max filter in std calculations:
    ## --> aims to limit standard deviations (to prevent unreasonable values for small profiles, or even breaking of function if sd == 0):
    ## compute relative quantities  (this procedure already corresponds to step 1 of layer-wise RTA computation: upper interface)
    bolmat <- matrix(nrow = nrow(lyrs), ncol = 6)
    colnames(bolmat) <- c("gtype", "hardness", "gsize", "dhardness", "dgsize", "depth")
    bolmat[, "gtype"] <- lyrs$gtype %in% thresh_gtype
    bolmat[, "hardness"] <- (lyrs$hardness - hbar) / max(sd_sample_uncorrected(lyrs$hardness, hbar), 1, na.rm = TRUE)
    bolmat[, "gsize"] <- (lyrs$gsize - gbar) / max(sd_sample_uncorrected(lyrs$gsize, gbar), 1, na.rm = TRUE)
    bolmat[, "dhardness"] <- (dhardness_upper - dhbar) / max(sd_sample_uncorrected(lyrs$dhardness_upper, dhbar), 1, na.rm = TRUE)
    bolmat[, "dgsize"] <- (dgsize_upper - dgbar) / max(sd_sample_uncorrected(na.omit(dgsize_upper), dgbar), 1, na.rm = TRUE)
    # bolmat[, "depth"] <- lyrs$depth < thresh_depth
    bolmat[, "depth"] <- (weibull_adjusted- weibullbar) / max(sd_sample_uncorrected(weibull_adjusted, weibullbar), 1, na.rm = TRUE)

    ## compute min and max of relative quantities
    minmax_h <- c(min(bolmat[, "hardness"]), max(bolmat[, "hardness"]))
    minmax_dh <- c(min(bolmat[, "dhardness"], na.rm = TRUE), max(bolmat[, "dhardness"], na.rm = TRUE))
    minmax_g <- c(min(bolmat[, "gsize"]), max(bolmat[, "gsize"]))
    minmax_dg <- c(min(bolmat[, "dgsize"], na.rm = TRUE), max(bolmat[, "dgsize"], na.rm = TRUE))
    minmax_dp <- c(min(bolmat[, "depth"]), max(bolmat[, "depth"]))

    ## compute difference of maxmin (and limit to > 1):
    minmax_diff_h <- max((minmax_h[2] - minmax_h[1]), 1)
    minmax_diff_g <- max((minmax_g[2] - minmax_g[1]), 1)
    minmax_diff_dh <- max((minmax_dh[2] - minmax_dh[1]), 1)
    minmax_diff_dg <- max((minmax_dg[2] - minmax_dg[1]), 1)
    minmax_diff_dp <- max((minmax_dp[2] - minmax_dp[1]), 1)

    if ("layer" %in% target) {
      ## scale relative quantities to [0, 1]
      bolmat[, "hardness"] <- 1 - ((bolmat[, "hardness"] - minmax_h[1]) / minmax_diff_h)  # note "1-..": weakest hardness = 0 -> invert
      bolmat[, "gsize"] <- (bolmat[, "gsize"] - minmax_g[1]) / minmax_diff_g
      bolmat[, "dhardness"] <- (bolmat[, "dhardness"] - minmax_dh[1]) / minmax_diff_dh
      bolmat[, "dgsize"] <- (bolmat[, "dgsize"] - minmax_dg[1]) / minmax_diff_dg
      bolmat[, "depth"] <- (bolmat[, "depth"] - minmax_dp[1]) / minmax_diff_dp

      ## compute relative quantities part II: lower interface
      bolmat_below <- bolmat
      bolmat_below[, "dhardness"] <- (dhardness_lower - dhbar) / max(sd_sample_uncorrected(na.omit(dhardness_lower), dhbar), 1, na.rm = TRUE)
      bolmat_below[, "dgsize"] <- (dgsize_lower - dgbar) / max(sd_sample_uncorrected(na.omit(dgsize_lower), dgbar), 1, na.rm = TRUE)
      ## scale relative quantities to [0, 1]: part II
      bolmat_below[, "dhardness"] <- (bolmat_below[, "dhardness"] - minmax_dh[1]) / minmax_diff_dh
      bolmat_below[, "dgsize"] <- (bolmat_below[, "dgsize"] - minmax_dg[1]) / minmax_diff_dg
      bolmat_below[, "depth"] <- (bolmat_below[, "depth"] - minmax_dp[1]) / minmax_diff_dp

      lyrs$rta <- pmax(rowSums(bolmat, na.rm = FALSE), rowSums(bolmat_below, na.rm = FALSE), na.rm = TRUE)
      ## normalization: either by theoretical max value (= 6), or by max value within profile (= implementation in SNOWPACK)
      lyrs$rta <- lyrs$rta / max(lyrs$rta, na.rm = TRUE)
    }

    if ("interface" %in% target) {
      ## compute relative quantities
      ## i) using layer properties from layer above
      bolmat_above <- matrix(nrow = nrow(lyrs), ncol = 6)
      colnames(bolmat_above) <- c("gtype", "hardness", "gsize", "dhardness", "dgsize", "depth")
      bolmat_above[, "gtype"] <- c(lyrs$gtype[-1], NA) %in% thresh_gtype
      bolmat_above[, "hardness"] <- (c(lyrs$hardness[-1], NA) - hbar) / max(sd_sample_uncorrected(c(lyrs$hardness[-1]), hbar), 1, na.rm = TRUE)
      bolmat_above[, "gsize"] <- (c(lyrs$gsize[-1], NA) - gbar) / max(sd_sample_uncorrected(c(lyrs$gsize[-1]), gbar), 1, na.rm = TRUE)
      bolmat_above[, "dhardness"] <- (dhardness_lower - dhbar) / max(sd_sample_uncorrected(na.omit(dhardness_lower), dhbar), 1, na.rm = TRUE)
      bolmat_above[, "dgsize"] <- (dgsize_lower - dgbar) / max(sd_sample_uncorrected(na.omit(dgsize_lower), dgbar), 1, na.rm = TRUE)
      # bolmat_above[, "depth"] <- c(lyrs$depth[-1], NA) < thresh_depth
      bolmat_above[, "depth"] <- (c(weibull_adjusted[-1], NA) - weibullbar) / max(sd_sample_uncorrected(weibull_adjusted[-1], weibullbar), 1, na.rm = TRUE)

      ## ii) using layer properties from layer below
      bolmat_below[, "gtype"] <- lyrs$gtype %in% thresh_gtype
      bolmat_below[, "hardness"] <- (lyrs$hardness - hbar) / max(sd_sample_uncorrected(lyrs$hardness, hbar), 1, na.rm = TRUE)
      bolmat_below[, "gsize"] <- (lyrs$gsize - gbar) / max(sd_sample_uncorrected(lyrs$gsize, gbar), 1, na.rm = TRUE)
      bolmat_below[, "dhardness"] <- (dhardness_lower - dhbar) / max(sd_sample_uncorrected(na.omit(dhardness_lower), dhbar), 1, na.rm = TRUE)
      bolmat_below[, "dgsize"] <- (dgsize_lower - dgbar) / max(sd_sample_uncorrected(na.omit(dgsize_lower), dgbar), 1, na.rm = TRUE)
      # bolmat_below[, "depth"] <- lyrs$depth < thresh_depth
      bolmat_below[, "depth"] <- (weibull_adjusted- weibullbar) / max(sd_sample_uncorrected(weibull_adjusted, weibullbar), 1, na.rm = TRUE)

      ## scale relative quantities to [0, 1]
      bolmat_above[, "hardness"] <- 1 - (bolmat_above[, "hardness"] - minmax_h[1]) / minmax_diff_h
      bolmat_above[, "gsize"] <- (bolmat_above[, "gsize"] - minmax_g[1]) / minmax_diff_g
      bolmat_above[, "dhardness"] <- (bolmat_above[, "dhardness"] - minmax_dh[1]) / minmax_diff_dh
      bolmat_above[, "dgsize"] <- (bolmat_above[, "dgsize"] - minmax_dg[1]) / minmax_diff_dg
      bolmat_above[, "depth"] <- (bolmat_above[, "depth"] - minmax_dp[1]) / minmax_diff_dp

      bolmat_below[, "hardness"] <- 1 - (bolmat_below[, "hardness"] - minmax_h[1]) / minmax_diff_h
      bolmat_below[, "gsize"] <- (bolmat_below[, "gsize"] - minmax_g[1]) / minmax_diff_g
      bolmat_below[, "dhardness"] <- (bolmat_below[, "dhardness"] - minmax_dh[1]) / minmax_diff_dh
      bolmat_below[, "dgsize"] <- (bolmat_below[, "dgsize"] - minmax_dg[1]) / minmax_diff_dg
      bolmat_below[, "depth"] <- (bolmat_below[, "depth"] - minmax_dp[1]) / minmax_diff_dp


      lyrs$rta_interface <- pmax(rowSums(bolmat_above, na.rm = FALSE), rowSums(bolmat_below, na.rm = FALSE), na.rm = FALSE)
      lyrs$rta_interface <- lyrs$rta_interface / max(lyrs$rta_interface, na.rm = TRUE)
    }
  }

  x$layers <- lyrs
  return(x)
}


