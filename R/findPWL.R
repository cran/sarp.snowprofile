#' Find layers of interest (e.g. PWLs) in snowprofile(Layers)
#'
#' Find one or more layers of interest, such as persistent weak layers (PWL) in a snowprofile or snowprofileLayers object based on combinations of grain type, datetag, grain size,
#' and stability indices (TSA/ RTA/ critical crack length/ p_unstable) of the layer. The routine can also be used for searching for crusts (or any other grain types).
#'
#' In case date considerations are included in your search, either one of the date window conditions needs to be satisfied to return a given layer:
#'
#'  - `ddate` or `datetag` within `date_range`, **or**
#'  - `bdate` within `bdate_range`
#'
#'  If the input object contains deposition dates (`ddate`, mostly in simulated profiles),
#' but no `bdates`, they are automatically computed by [deriveDatetag];
#' otherwise the date window is applied to the `datetag` (mostly for manual profiles).
#'
#' If you apply thresholds to your search, only layers are returned that satisfy *at least one* of the provided thresholds.
#'
#' @describeIn findPWL Find layers of interest (e.g., PWLs) in `snowprofile` or `snowprofileLayers`
#' @param x [snowprofile] or [snowprofileLayers] object
#' @param pwl_gtype a vector of grain types of interest
#' @param pwl_date a date of interest given as character ('YYYY-MM-DD') or as POSIXct; set to `NA` to ignore dates. If given as POSIXct, time comparison between layer dates and pwl_date
#' will consider the times of day (i.e., hours, etc). Otherwise only consider year/month/days.
#' @param date_range a numeric array of length 2 that defines a date search window around `pwl_date`. This date range is applied to `ddate`s (deposition dates),
#' or if these are not available to `datetag`s.
#' @param date_range_earlier a [difftime] object of `date_range[1]` (must be negative).
#' @param date_range_later a [difftime] object of `date_range[2]` (must be positive).
#' @param bdate_range a numeric array of length 2 that defines a date search window around `pwl_date`. This date range is applied to `bdate`s (burial dates)
#' @param bdate_range_earlier a [difftime] object of `bdate_range[1]` (must be negative).
#' @param bdate_range_later a [difftime] object of `bdate_range[2]` (must be positive).
#' @param threshold_gtype specific grain types that are only deemed a PWL if they pass one or multiple thresholds (see next parameters)
#' @param threshold_gsize a threshold grain size in order to deem `threshold_gtype` a PWL; set to `NA` to ignore grain sizes.
#' @param threshold_TSA a threshold TSA value (see [computeTSA]) in order to deem `threshold_gtype` a PWL; set to `NA` to ignore TSA.
#' @param threshold_RTA a threshold RTA value (see [computeRTA]) in order to deem `threshold_gtype` a PWL; set to `NA` to ignore RTA.
#' @param threshold_SK38 a threshold SK38 in order to deem `threshold_gtype` a PWL; set to `NA` to ignore this threshold.
#' @param threshold_RC a threshold critical crack length in order to deem `threshold_gtype` a PWL; set to `NA` to ignore this threshold.
#' @param threshold_PU a threshold value for p_unstable in order to deem `threshold_gtype` a PWL; set to `NA` to ignore this threshold.
#'
#' @return `findPWL`: An index vector of PWLs that match the desired requirements
#'
#' @author fherla
#'
#' @examples
#' ## get index vector:
#' findPWL(SPpairs$A_modeled)
#'
#' ## get layers subset:
#' SPpairs$A_manual$layers[findPWL(SPpairs$A_manual), ]
#' SPpairs$A_manual$layers[findPWL(SPpairs$A_manual, threshold_gsize = 2.2,
#'                         threshold_gtype = c("FC", "FCxr")), ]
#' ## all (SH, DH), and (FC, FCxr) >= 1 mm grain size:
#' SPpairs$A_modeled$layers[findPWL(SPpairs$A_modeled, pwl_gtype = c("SH", "DH", "FC", "FCxr"),
#'                                  threshold_gsize = 1, threshold_gtype = c("FC", "FCxr")), ]
#' ## use TSA threshold:
#' SPpairs$A_modeled <- computeTSA(SPpairs$A_modeled)
#' SPpairs$A_modeled$layers[findPWL(SPpairs$A_modeled, pwl_gtype = c("SH", "DH", "FC", "FCxr"),
#'                                  threshold_TSA = 4, threshold_gtype = c("FC", "FCxr")), ]
#'
#' ## searching for a specific pwl_date:
#' ## let's construct one layer and an array of pwl_dates
#' tl <- snowprofileLayers(height = 1, gtype = "SH",
#'                         ddate = as.POSIXct("2020-12-15"),
#'                         bdate = as.POSIXct("2020-12-20"))
#' pwl_dates <- paste0("2020-12-", seq(14, 22))
#' ## which pwl_date will 'find' that layer?
#' sapply(pwl_dates, function(dt) length(findPWL(tl, pwl_date = dt)) > 0)
#' ## same example, but with bdate being NA:
#' tl <- snowprofileLayers(height = 1, gtype = "SH",
#'                         ddate = as.POSIXct("2020-12-15"),
#'                         bdate = as.POSIXct(NA), dropNAs = FALSE)
#' sapply(pwl_dates, function(dt) length(findPWL(tl, pwl_date = dt)) > 0)
#'
#' ## pwl_date example with proper profile:
#' sp <- deriveDatetag(SPpairs$A_manual)
#' sp$layers
#' pwl_dates <- paste0("2019-02-", seq(18, 26))
#' names(pwl_dates) <- pwl_dates
#' ## which pwl_date will 'find' the two layers with (b)date labels?
#' list(pwl_date = lapply(pwl_dates, function(dt) {
#'   sp$layers[findPWL(sp, pwl_gtype = c("SH", "FC"), pwl_date = dt),
#'             c("height", "gtype", "ddate", "bdate")]
#' }))
#'
#' ## same example as above, but including TSA threshold:
#' sp <- computeTSA(sp)
#' ## the SH layer has TSA 5, the FC layer has TSA 4:
#' list(pwl_date = lapply(pwl_dates, function(dt) {
#'   sp$layers[findPWL(sp, pwl_gtype = c("SH", "FC"), pwl_date = dt, threshold_TSA = 5),
#'             c("height", "gtype", "ddate", "bdate")]
#' }))
#' ## --> no more FC layer in output since its TSA value is below the threshold!
#'
#' ## can also be used to search for crusts:
#' SPpairs$A_manual$layers[findPWL(SPpairs$A_manual, pwl_gtype = "MFcr"), ]
#'
#' @export
#'
findPWL <- function(x,
                    pwl_gtype = c("SH", "DH"),
                    pwl_date = NA,
                    date_range = c(-5, 0),
                    date_range_earlier = as.difftime(date_range[1], units = "days"),
                    date_range_later = as.difftime(date_range[2], units = "days"),
                    bdate_range = c(-1, 1),
                    bdate_range_earlier = as.difftime(bdate_range[1], units = "days"),
                    bdate_range_later = as.difftime(bdate_range[2], units = "days"),
                    threshold_gtype = pwl_gtype,
                    threshold_gsize = NA,
                    threshold_TSA = NA,
                    threshold_RTA = NA,
                    threshold_SK38 = NA,
                    threshold_RC = NA,
                    threshold_PU = NA) {


  ## Assertions
  if (is.snowprofile(x)) layers <- x$layers
  else if (is.snowprofileLayers(x)) layers <- x
  else stop("x needs to be a snowprofile or snowprofileLayers object")

  if (!inherits(date_range_earlier, "difftime") | !inherits(date_range_later, "difftime"))
    stop("date_range needs to be given as difftime object")
  if (!inherits(bdate_range_earlier, "difftime") | !inherits(bdate_range_later, "difftime"))
    stop("bdate_range needs to be given as difftime object")

  threshold_gtype <- intersect(pwl_gtype, threshold_gtype)

  ## Structural conditions that satisfy a pwl
  PwlRows <- layers$gtype %in% pwl_gtype
  ## initialize other thresholds
  nL_threshold <- sum(layers$gtype %in% threshold_gtype)
  gsRows <- rep(NA, nL_threshold)
  tsaRows <- rep(NA, nL_threshold)
  rtaRows <- rep(NA, nL_threshold)
  skRows <- rep(NA, nL_threshold)
  rcRows <- rep(NA, nL_threshold)
  puRows <- rep(NA, nL_threshold)
  mustMerge <- FALSE
  ## gsize
  if (!is.na(threshold_gsize)) {
    gsize_avail <- "gsize" %in% names(layers)
    if (!gsize_avail) {
      if ("gsize_max" %in% names(layers)) {
        gsize_avail <- TRUE
        layers[, "gsize"] <- layers[, "gsize_max"]
      } else {
        warning(paste0("No 'gsize' available in profile!"))
      }
    }
    if (gsize_avail) {
      mustMerge <- TRUE
      gsRows <- layers$gsize[layers$gtype %in% threshold_gtype] >= threshold_gsize
    }
  }
  ## TSA
  if (!is.na(threshold_TSA)) {
    if (!"tsa" %in% names(layers)) {
      warning("No 'tsa' available in profile!")
    } else {
      mustMerge <- TRUE
      tsaRows <- layers$tsa[layers$gtype %in% threshold_gtype] >= threshold_TSA
    }
  }
  ## RTA
  if (!is.na(threshold_RTA)) {
    if (!"rta" %in% names(layers)) {
      warning("No 'rta' available in profile!")
    } else {
      mustMerge <- TRUE
      rtaRows <- layers$rta[layers$gtype %in% threshold_gtype] >= threshold_RTA
    }
  }
  ## SK38
  if (!is.na(threshold_SK38)) {
    if (!"sk38" %in% names(layers)) {
      warning("No 'sk38' available in profile!")
    } else {
      mustMerge <- TRUE
      skRows <- layers$sk38[layers$gtype %in% threshold_gtype] <= threshold_SK38
    }
  }
  ## crit_cut_length
  if (!is.na(threshold_RC)) {
    if (!"crit_cut_length" %in% names(layers)) {
      warning("No 'crit_cut_length' available in profile!")
    } else {
      mustMerge <- TRUE
      rcRows <- layers$crit_cut_length[layers$gtype %in% threshold_gtype] <= threshold_RC
    }
  }
  ## p_unstable
  if (!is.na(threshold_PU)) {
    if (!"p_unstable" %in% names(layers)) {
      warning("No 'p_unstable' available in profile!")
    } else {
      mustMerge <- TRUE
      puRows <- layers$p_unstable[layers$gtype %in% threshold_gtype] >= threshold_PU
    }
  }

  ## merge thresholds
  if (mustMerge) {
    PwlRows[layers$gtype %in% threshold_gtype] <- PwlRows[layers$gtype %in% threshold_gtype] & (gsRows | tsaRows | rtaRows | skRows | rcRows | puRows)
    PwlRows[is.na(PwlRows)] <- FALSE
  }


  ## date considerations for pwl search
  if (!is.na(pwl_date)) {  # pwl_date needs to be considered
    if (date_range_earlier > 0) {
      warning("date_range_earlier must be < 0! Multiplying by '-1'..")
      date_range_earlier <- -date_range_earlier
    }
    if (bdate_range_earlier > 0) {
      warning("bdate_range_earlier must be < 0! Multiplying by '-1'..")
      bdate_range_earlier <- -bdate_range_earlier
    }
    if ("ddate" %in% names(layers)) {  # ddate available --> derive bdate
      if (!"bdate" %in% names(layers)) layers <- deriveDatetag.snowprofileLayers(layers, checkMonotonicity = FALSE)
      if (!inherits(pwl_date, "POSIXct")) {   # convert to Date if pwl_date not given in POSIXct format
        layers$bdate <- as.Date(as.character(layers$bdate))  # double conversion ensures that timezone won't change from POSIXct to Date format!
        layers$ddate <- as.Date(as.character(layers$ddate))
        pwl_date <- as.Date(as.character(pwl_date))
      }
      ## intersect of PwlRows and ddate/bdate ranges
      idx <- which(PwlRows &
                     (((layers$ddate - pwl_date) >= as.difftime(date_range_earlier, units = "days") &
                        (layers$ddate - pwl_date) <= as.difftime(date_range_later, units = "days") ) |
                     ((layers$bdate - pwl_date) >= as.difftime(bdate_range_earlier, units = "days") &
                        (layers$bdate - pwl_date) <= as.difftime(bdate_range_later, units = "days"))))
      return(idx)
    } else if ("datetag" %in% names(layers)) {
      layers$datetag <- as.Date(as.character(layers$datetag))
      pwl_date <- as.Date(as.character(pwl_date))
      idx <- which(PwlRows &
                     ((layers$datetag - pwl_date) >= as.difftime(date_range_earlier, units = "days")) &
                     ((layers$datetag - pwl_date) <= as.difftime(date_range_later, units = "days")))
      return(idx)
    } else {
      warning("Neither 'ddate' nor 'datetag' available in profile, ignoring pwl_date..")
    }
  }

  return(which(PwlRows))

}
