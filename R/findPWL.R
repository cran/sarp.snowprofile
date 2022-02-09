#' Find layers of interest (e.g. PWLs) in snowprofile(Layers)
#'
#' Find one or more layers of interest, such as persistent weak layers (PWL) in a snowprofile or snowprofileLayers object based on combinations of grain type, datetag, grain size,
#' and stability indices (TSA/ RTA) of the layer. The routine can also be used for searching for crusts (or any other grain types).
#'
#' In case date considerations are included in your search: at least one of the two conditions *(b)date_range_earlier* **AND** at least one
#' of the two conditions *(b)date_range_later* needs to be satisfied to return a given layer. If the input object contains deposition dates (`ddate`, mostly in simulated profiles),
#' then the date window is applied to both `ddate` and a newly computed `bdate`;
#' otherwise the date window is applied to the `datetag` (mostly for manual profiles).
#'
#' @describeIn findPWL Find layers of interest (e.g., PWLs) in `snowprofile` or `snowprofileLayers`
#' @param x [snowprofile] or [snowprofileLayers] object
#' @param pwl_gtype a vector of grain types of interest
#' @param pwl_date a date of interest given as character ('YYYY-MM-DD') or as POSIXct; set to `NA` to ignore dates.
#' @param date_range_earlier a [difftime] object that defines a date search window prior to `pwl_date`. This date range is applied to `ddate`s (deposition dates),
#' or if these are not available to `datetag`s.
#' @param date_range_later analogous to `date_range_earlier`
#' @param bdate_range_earlier analogous to `date_range_earlier`, but applied to `bdate`s (burial dates).
#' @param bdate_range_later analogous to `bdate_range_earlier`.
#' @param threshold_gtype specific grain types that are only deemed a PWL if they pass a certain threshold (see next parameters)
#' @param threshold_gsize a threshold grain size in order to deem `threshold_gtype` a PWL; set to `NA` to ignore grain sizes.
#' @param threshold_TSA a threshold TSA value (see [computeTSA]) in order to deem `threshold_gtype` a PWL; set to `NA` to ignore TSA.
#' @param threshold_RTA a threshold RTA value (see [computeRTA]) in order to deem `threshold_gtype` a PWL; set to `NA` to ignore RTA.
#' @param threshold_NA if the threshold you want to apply to `threshold_gtype` is `NA`, do you then want to deem the layer a PWL (i.e., set `TRUE`) or not (i.e., set to `FALSE`)?
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
#' SPpairs$A_manual$layers[findPWL(SPpairs$A_manual, pwl_date = "2019-02-18"), ]
#' ## Why is there a layer included without date information?!
#' ## --> because the layers 'bdate' can be computed from the known 'ddate' of the layer above
#' ## and the resulting 'bdate' lies within the allowed 'date_range'...
#'
#' ## can also be used to search for crusts:
#' SPpairs$A_manual$layers[findPWL(SPpairs$A_manual, pwl_gtype = "MFcr"), ]
#'
#' @export
#'
findPWL <- function(x,
                    pwl_gtype = c("SH", "DH"),
                    pwl_date = NA,
                    date_range_earlier = as.difftime(5, units = "days"),
                    date_range_later = as.difftime(0, units = "days"),
                    bdate_range_earlier = as.difftime(1, units = "days"),
                    bdate_range_later = as.difftime(1, units = "days"),
                    threshold_gtype = pwl_gtype,
                    threshold_gsize = NA,
                    threshold_TSA = NA,
                    threshold_RTA = NA,
                    threshold_NA = TRUE) {


  ## Assertions
  if (is.snowprofile(x)) layers <- x$layers
  else if (is.snowprofileLayers(x)) layers <- x
  else stop("x needs to be a snowprofile or snowprofileLayers object")

  if (!inherits(date_range_earlier, "difftime") | !inherits(date_range_later, "difftime"))
    stop("date_range needs to be given as difftime object")
  if (!inherits(bdate_range_earlier, "difftime") | !inherits(bdate_range_later, "difftime"))
    stop("bdate_range needs to be given as difftime object")

  threshold_gtype <- intersect(pwl_gtype, threshold_gtype)
  if (sum(as.logical(c(threshold_gsize, threshold_TSA, threshold_RTA)), na.rm = TRUE) > 1) stop("Can only provide ONE of the following: threshold_gsize, .._TSA, .._RTA!")

  # Structural conditions that satisfy a pwl
  PwlRows <- layers$gtype %in% pwl_gtype
  if (!is.na(threshold_gsize)) {
    gsize_avail <- "gsize" %in% names(layers)
    if (!gsize_avail) {
      if ("gsize_max" %in% names(layers)) {
        gsize_avail <- TRUE
        layers[, "gsize"] <- layers[, "gsize_max"]
      }
    }
    if (gsize_avail) {
      PwlRows[layers$gtype %in% threshold_gtype] <- layers$gsize[layers$gtype %in% threshold_gtype] >= threshold_gsize
      PwlRows[is.na(PwlRows)] <- threshold_NA
    } else {
      PwlRows[layers$gtype %in% threshold_gtype] <- threshold_NA
      warning(paste0("No 'gsize' available in profile, ", ifelse(threshold_NA, "still including ", "discarding "), "following grain type(s) as PWL: ", paste(threshold_gtype, collapse = ", ")))
    }
  } else if (!is.na(threshold_TSA)) {
    if (!"tsa" %in% names(layers)) stop("snowprofileLayers property 'tsa' not available")
    PwlRows[layers$gtype %in% threshold_gtype] <- layers$tsa[layers$gtype %in% threshold_gtype] >= threshold_TSA
    PwlRows[is.na(PwlRows)] <- threshold_NA
  } else if (!is.na(threshold_RTA)) {
    if (!"rta" %in% names(layers)) stop("snowprofileLayers property 'rta'  not available")
    PwlRows[layers$gtype %in% threshold_gtype] <- layers$rta[layers$gtype %in% threshold_gtype] >= threshold_RTA
    PwlRows[is.na(PwlRows)] <- threshold_NA
  }

  ## date considerations for pwl search
  if (!is.na(pwl_date)) {  # pwl_date needs to be considered
    if ("ddate" %in% names(layers)) {  # ddate available --> derive bdate and datetag
      layers <- deriveDatetag(layers)
      layers$bdate <- as.Date(as.character(layers$bdate))  # double conversion ensures that timezone won't change from POSIXct to Date format!
      layers$ddate <- as.Date(as.character(layers$ddate))
      pwl_date <- as.Date(as.character(pwl_date))
      ## intersect of PwlRows and ddate/bdate ranges
      idx <- which(PwlRows &
                     (pwl_date - layers$ddate <= as.difftime(date_range_earlier, units = "days") |
                        pwl_date - layers$bdate <= as.difftime(bdate_range_earlier, units = "days") ) &
                     (layers$ddate - pwl_date <= as.difftime(date_range_later, units = "days") |
                        layers$bdate - pwl_date <= as.difftime(bdate_range_later, units = "days")))
      return(idx)
    } else if ("datetag" %in% names(layers)) {
      layers$datetag <- as.Date(as.character(layers$datetag))
      pwl_date <- as.Date(as.character(pwl_date))
      idx <- which(PwlRows &
                     (pwl_date - layers$datetag <= as.difftime(date_range_earlier, units = "days")) &
                     (layers$datetag - pwl_date <= as.difftime(date_range_later, units = "days")))
      return(idx)
    } else {
      warning("Neither 'ddate' nor 'datetag' available in profile, ignoring pwl_date..")
    }
  }

  return(which(PwlRows))

}
