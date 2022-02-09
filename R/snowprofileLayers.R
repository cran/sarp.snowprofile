## ######################################################
##
## snowprofileLayers class definition, following guidelines in:
## https://adv-r.hadley.nz/s3.html
##
## ######################################################
## authors: shorton, fherla

#' Check class snowprofileLayers
#'
#' Check if object is of class snowprofileLayers
#'
#' @param x object to test
#'
#' @return boolean
#'
#' @export
#'
is.snowprofileLayers <- function(x) inherits(x, "snowprofileLayers")


#' Constructor for a snowprofileLayers object
#'
#' Helper function to conveniently create a snowprofileLayers object, i.e. data.frame with mandatory column fields height (or depth) that provides vertical position of layers.
#' Layers need to be ordered in a sequential manner, and the routine will rearrange the layers so that the last row of the resulting dataframe corresponds to the snow surface.
#' If the vertical location of the layers is given by depth, make sure to provide `hs` if it's known. Otherwise, provide the field `maxObservedDepth` or layer thicknesses.
#' Providing only depth will issue a warning and set the corresponding lowest layer thickness to NA.
#' The resulting dataframe will contain all three fields `height`, `depth`, and `thickness`, which will be auto-filled if not provided (see [format_snowprofileLayers]).
#' If the columns that describe layer properties are not of equal
#' lengths, their values will be recycled (default data.frame mechanism). Instead of individual layer characteristics, a data.frame can be provided, which will be converted into a snowprofileLayers class.
#' The constructor asserts correctness of the layers object by a call to [validate_snowprofileLayers].
#'
#' @param height height vector (cm) referring to the top layer interface. Instead of `height`, `depth` can also be given and should be accompanied by
#' an array specifying the `thickness` of the layers, or alternatively, the total snow depth `hs` and/or the maximum observed depth `maxObservedDepth`
#' should be provided. Note, that also the `depth` refers to the top layer interface. **See examples!**
#' @param temperature snow temperature (deg C)
#' @param density layer density (kg/m3)
#' @param lwc liquid water content (%)
#' @param gsize grain size (mm)
#' @param gsize_max maximum grain size (mm)
#' @param gsize_avg average grain size (mm)
#' @param gtype grain type (character or factor)
#' @param gtype_sec secondary grain type (character or factor)
#' @param hardness numeric hand hardness (use [char2numHHI] to convert from character hardness)
#' @param ddate deposition date of layer (POSIXct format)
#' @param bdate burial date of layer (Date format)
#' @param datetag of layer (i.e., usually corresponds to `ddate` for 'MFcr', and to `bdate` for all other grain types.)
#' @param ssi snow stability index (numeric)
#' @param sphericity between 0 and 1
#' @param v_strain_rate viscous deformation rate (s^-1)
#' @param crit_cut_length critical crack length (m)
#' @param tsa threshold sum approach for structural instability (also called lemons); valid for the layer, i.e., the weakest interface adjacent to the layer. see [computeTSA].
#' @param tsa_interface same as tsa, but valid for top interface of corresponding layer
#' @param rta relative threshold sum approach (following Monti et al 2013, ISSW paper); valid for the layer, i.e., the weakest interface adjacent to the layer. see [computeRTA].
#' @param rta_interface same as rta, but valid for top interface of corresponding layer
#' @param layerOfInterest a boolean column to label specific layers of interest, e.g. weak layers. see [labelPWL].
#' @param comment character string
#' @param ... columns to include in the layers object. Note, that they need to correspond to the according height/depth array.
#' e.g. hardness (can use character hardness or numeric hardness via [char2numHHI]), ddate (class POSIX), bdate (class Date) gtype (character or factor), density, temperature, gsize, lwc, gsize_max, gtype_sec, ssi, depth, thickness
#' @param hs total snow height (cm), if not deductible from `height` vector. Particularly important when only a depth grid is provided!
#' @param maxObservedDepth the observed depth of the profile from the snow surface downwards. Will only be used, if
#' no `height`, `thickness`, or `hs` is given.
#' @param layerFrame a data.frame that's converted to a snowprofileLayers class if no other layer characteristics are provided
#' @param validate Validate `obj` with [validate_snowprofileLayers]?
#' @param dropNAs Do you want to drop all columns consisting of NAs only?
#'
#' @return snowprofileLayers object as data.frame with strings as factors
#'
#' @seealso [snowprofile]
#'
#' @author shorton, fherla
#'
#' @examples
#'
#' ## Empty layers object:
#' snowprofileLayers()
#'
#'
#' ## simple layers example that recycles the hardness 1F+: with warning issued!
#' snowprofileLayers(height = c(10, 25, 50),
#'                   hardness = char2numHHI('1F+'),
#'                   gtype = c('FC', NA, 'PP'))
#'
#'
#' ## create snowprofileLayers object from data.frame
#' ## and feed it into a snowprofile object:
#' df <- data.frame(height = c(10, 25, 50),
#'                   hardness = c(2, 3, 1),
#'                   gtype = c('FC', NA, 'PP'),
#'                   stringsAsFactors = TRUE)
#'
#' spL <- snowprofileLayers(layerFrame = df)
#' (sp <- snowprofile(layers = spL))
#'
#'
#' ##### Create top-down recorded snowprofileLayers ####
#' ## check out how the fields 'hs' and 'maxObservedDepth' are auto-filled in the
#' ## resulting snowprofile object!
#' ## 1.) Specify depth and hs:
#' ## In that case the routine will assume that the deepest layer extends down to the ground
#' (sp1 <- snowprofile(layers = snowprofileLayers(depth = c(40, 25, 0),
#'                                                hardness = c(2, 3, 1),
#'                                                gtype = c('FC', NA, 'PP'),
#'                                                hs = 50)))
#' ## note that sp and sp1 are the same profiles:
#' all(sapply(names(sp$layers), function(cols) {sp$layers[cols] == sp1$layers[cols]}), na.rm = TRUE)
#'
#' ## 2.) Specify depth, hs and thickness or maxObservedDepth:
#' ## This will include a basal layer of NAs to fill the unobserved space down to the ground.
#' (sp2 <- snowprofile(layers = snowprofileLayers(depth = c(40, 25, 0),
#'                                                hardness = c(2, 3, 1),
#'                                                gtype = c('FC', NA, 'PP'),
#'                                                hs = 70,
#'                                                maxObservedDepth = 50)))
#'
#' ## 3.) Specify depth and maxObservedDepth:
#' ## This will include a basal layer of NAs which is 1 cm thick to flag the unknown basal layers.
#' (sp3 <- snowprofile(layers = snowprofileLayers(depth = c(40, 25, 0),
#'                          hardness = c(2, 3, 1),
#'                          gtype = c('FC', NA, 'PP'),
#'                          gsize = c(2, NA, NA),
#'                          maxObservedDepth = 50)))
#'
#' ## 4.) Specify depth and thickness:
#' ## This is equivalent to the example spL3 above!
#' ## This will include a basal layer of NAs which is 1 cm thick to flag the unknown basal layers.
#' (sp4 <- snowprofile(layers = snowprofileLayers(depth = c(40, 25, 0),
#'                          thickness = c(10, 15, 25),
#'                          hardness = c(2, 3, 1),
#'                          gtype = c('FC', NA, 'PP'))))
#'
#' ## 5.) Specify only depth: issues warning!
#' (sp5 <- snowprofile(layers = snowprofileLayers(depth = c(40, 25, 0),
#'                          hardness = c(2, 3, 1),
#'                          gtype = c('FC', NA, 'PP'))))
#'
#' ## plot all 5 top.down-recorded profiles:
#' set <- snowprofileSet(list(sp1, sp2, sp3, sp4, sp5))
#' plot(set, SortMethod = "unsorted", xticklabels = "originalIndices",
#'      hardnessResidual = 0.1, hardnessScale = 1.5, TopDown = TRUE,
#'      main = "TopDown Plot")
#'
#' plot(set, SortMethod = "unsorted", xticklabels = "originalIndices",
#'      hardnessResidual = 0.1, hardnessScale = 1.5, TopDown = FALSE,
#'      main = "BottomUp Plot")
#'
#' @export
#'
snowprofileLayers <- function(height = as.double(NA),
                              temperature = as.double(NA),
                              density = as.double(NA),
                              lwc = as.double(NA),
                              gsize = as.double(NA),
                              gsize_max = as.double(NA),
                              gsize_avg = as.double(NA),
                              gtype = as.factor(NA),
                              gtype_sec = as.factor(NA),
                              hardness = as.double(NA),
                              ddate = as.POSIXct(NA),
                              bdate = as.Date(NA),
                              datetag = as.POSIXct(NA),
                              ssi = as.double(NA),
                              sphericity = as.double(NA),
                              v_strain_rate = as.double(NA),
                              crit_cut_length = as.double(NA),
                              tsa = as.double(NA),
                              tsa_interface = as.double(NA),
                              rta = as.double(NA),
                              rta_interface = as.double(NA),
                              layerOfInterest = as.logical(NA),
                              comment = as.character(NA),
                              ...,
                              hs = as.double(NA),
                              maxObservedDepth = as.double(NA),
                              layerFrame = NA,
                              validate = TRUE,
                              dropNAs = TRUE) {

  ## combine layer properties into one list
  entries <- c(list(...), list(height = height,
                               temperature = temperature,
                               density = density,
                               lwc = lwc,
                               gsize = gsize,
                               gsize_max = gsize_max,
                               gsize_avg = gsize_avg,
                               gtype = gtype,
                               gtype_sec = gtype_sec,
                               hardness = hardness,
                               ddate = ddate,
                               bdate = bdate,
                               datetag = datetag,
                               ssi = ssi,
                               sphericity = sphericity,
                               v_strain_rate = v_strain_rate,
                               crit_cut_length = crit_cut_length,
                               tsa = tsa,
                               tsa_interface = tsa_interface,
                               rta = rta,
                               rta_interface = rta_interface,
                               layerOfInterest = layerOfInterest,
                               comment = comment))


  ## create snowprofile object either from specific input variables or from layerFrame:
  if (all(is.na(entries)) && is.data.frame(layerFrame)) {
    object <- as.data.frame(layerFrame, stringsAsFactors = TRUE)

  } else {
    ## warn if recycling values due to different lengths of input vectors
    if (any(diff(sapply(entries[!is.na(entries)], length)) != 0)) warning("not all inputs have the same length, recycling..")
    ## create data.frame
    object <- data.frame(entries,
                         stringsAsFactors = TRUE)
  }
  ## assign class
  class(object) <- append("snowprofileLayers", class(object))

  ## run checks and format layers if object is not empty (i.e., one row of all NAs)
  if (!(nrow(object) == 1 & all(is.na(object[1, ])))) {
    object <- format_snowprofileLayers(object, target = "all", hs = hs, maxObservedDepth = maxObservedDepth,
                                       validate = validate, dropNAs = dropNAs)
  } else {
    ## append columns depth and thickness at positions 1 and 3
    object$depth <- as.double(NA)
    object$thickness <- as.double(NA)
    idcols <- c("depth", "height", "thickness")
    cols <- colnames(object)
    cols <- c(idcols, cols[-which(cols %in% idcols)])
    object <- object[, cols]
  }

  return(object)
}


#' Format snowprofileLayers
#'
#' Calculate missing data.frame columns based on the given ones, if possible.
#'
#' @param obj snowprofileLayers object
#' @param target string, indicating which fields are auto-filled ('all', 'height', 'depth', 'thickness', 'none')
#' @param hs total snow height (cm) if not deductible from given fields
#' @param maxObservedDepth the observed depth of the profile from the snow surface downwards.
#' Will only be used, if no `height` or `thickness` exist in `obj`, or if `hs` is not given.
#' @param validate Validate `obj` with [validate_snowprofileLayers]?
#' @param dropNAs Do you want to drop all columns consisting of NAs only?
#'
#' @return copy of obj with auto-filled columns
#'
#' @export
#'
format_snowprofileLayers <- function(obj, target = "all", hs = NA, maxObservedDepth = NA, validate = TRUE, dropNAs = TRUE) {

  ## ---Initial manipulations, etc----
  ## drop all columns that contain only NA values:
  if (dropNAs) object <- obj[, colSums(is.na(obj)) < nrow(obj)]
  else object <- obj
  cols <- colnames(object)
  hsmin <- ifelse("height" %in% cols, tail(object$height, 1), object$depth[1])
  if (all(!is.na(c(hs, hsmin)))) try(if (hs < hsmin) stop(paste("'hs' must be >=", hsmin, "cm")))

  ## sort snow surface to last row:
  if ("height" %in% cols) k <- order(object$height)
  else if ("depth" %in% cols) k <- order(object$depth, decreasing = TRUE)
  object <- object[k, ]

  ## decide which columns to calculate:
  if (target == "all") target <- c("height", "depth", "thickness")
  do <- target[!target %in% cols]
  if (is.null(nrow(object[, target[target %in% cols]]))) target_containNA <- NA
  else target_containNA <- names(which(sapply(object[, target[target %in% cols]], function(x) any(is.na(x)))))
  if (("height" %in% target_containNA) & ("depth" %in% target_containNA)) {
    stop("NAs in both height and depth grid! --> Ambiguous!")
  } else if ("height" %in% target_containNA) {
    do <- c(do, "height")
  } else if ("depth" %in% target_containNA) {
    do <- c(do, "depth")
  }

  ## initialize for later use:
  setBasalThicknessNA <- FALSE
  offset <- as.double(NA)

  ## ---Conversion of grids----
  if (any(do %in% "height")) {
    ## calculate offset for conversion:
    if (!is.na(maxObservedDepth)) {
      ## check whether reported maxObservedDepth is in line with reported thicknesses:
      if (c("thickness") %in% cols) {
        thickness_derived_maxObservedDepth <- object$depth[1] + object$thickness[1]
        maxObservedDepth <- max(c(maxObservedDepth, thickness_derived_maxObservedDepth), na.rm = TRUE)
      }
      offset <- maxObservedDepth
    }
    if (!is.na(hs)) offset <- hs  # this deliberately overrides previous offset in case of ambiguity
    ## maxObservedDepth and hs are both unknown: --> try getting maxObservedDepth from thickness:
    if (is.na(offset)) {
      if (c("thickness") %in% cols) {
        maxObservedDepth <- object$depth[1] + object$thickness[1]
        offset <- maxObservedDepth
      }
    }
    ## layer thickness is also unknown:
    if (is.na(offset)) {
      warning("Total snow height 'hs' is unknown, 'maxObservedDepth' is unknown, and layer 'thickness' of deepest layer is unknown!\n--> Setting lowest layer thickness to NA and maxObservedDepth to 10 cm below the lowest layer.")
      setBasalThicknessNA <- TRUE
      do <- c(do, "thickness")  # make sure thickness is computed below!
      offset <- object$depth[1] + 11
    }

    ## finally compute height vector:
    object$height <- offset - object$depth
  }


  if (any(do %in% "depth")) {  # i.e., height vector is known
    ## retrieve maximum snow height
    offset <- suppressWarnings( max(c(hs, maxObservedDepth, max(object$height)), na.rm = TRUE) )  # hack for bug in `max`!
    object$depth <- offset - object$height
  }

  if (any(do %in% "thickness")) {
    if (exists("height", where = object)) {
      offset4thickness <- suppressWarnings( max(c(0, hs - maxObservedDepth), na.rm = TRUE) )
      object$thickness <- diff(c(offset4thickness, object$height))
    } else {
      if (is.na(offset)) stop("Something went wrong! You likely need to provide more information, such as 'hs', 'maxObservedDepth', or 'thickness'!")
      offset4thickness <- ifelse(is.na(maxObservedDepth), offset, maxObservedDepth)
      object$thickness <- abs(diff(c(offset4thickness, object$depth)))
    }
    if (setBasalThicknessNA) object$thickness[1] <- as.double(NA)
  }

  ## ---Insert basal layer if necessary----
  ## The code below requires height and thickness,
  ## which should be available with new default formatTarget == "all" settings!
  if (!(exists("height", where = object) & exists("thickness", where = object))) {
    stop("Something went wrong and your profiles has no 'height' and/or 'thickness' column. Use formatTarget = 'all'!" )
  }

  ## insert NA basal layer if (height - thickness) of the lowest layer is above the ground, AND...
  basal_offset <- (object$height[1] - object$thickness[1])  # (height - thickness) of the lowest layer

  ## ... AND when depth is given but hs is unknown:
  if (!"depth" %in% do & is.na(hs)) {
    basal_offset <- 1
    setBasalThicknessNA <- TRUE
  }

  if (isTRUE(basal_offset >= 1)) {  # previous queries demand for basal offset layer
    object <- insertUnobservedBasalLayer(object, basal_offset, setBasalThicknessNA)
  }

  ## ---Cosmetcis: colorder----
  idcols <- c("depth", "height", "thickness")
  cols <- c(idcols, cols[-which(cols %in% idcols)])
  object <- object[, cols]

  ## ---Type conversions----
  emptySPL <- snowprofileLayers(validate = FALSE, dropNAs = FALSE)
  dtypes <- sapply(emptySPL, function(x) class(x)[1])
  for (col in cols) {  # convert standard object columns to correct classes
    if (col %in% names(dtypes)){
      if (unname(dtypes[col]) == "POSIXct" & !inherits(object[, col], "POSIXct")) {  # hack to prevent errors while converting empty strings to POSIXct
        object[which(object[, col] == ""), col] <- NA
      }
      object[, col] <- do.call(paste0("as.", unname(dtypes[col])), list(x = object[, col]))  # do type conversion
    }
  }

  ## ---Final assertions: validate input----
  if (validate) validate_snowprofileLayers(object)

  return(object)
}


#' Validate correctness of snowprofileLayers object
#'
#' Validator function that checks if class standards are being met and raises an error if not.
#'
#' @param object to be tested
#' @param silent remain silent upon error (i.e., don't throw error, but only print it)
#'
#' @return Per default an error is raised when discovered, if `silent = TRUE` the error is only printed and the
#' error message returned.
#'
#' @export
#'
validate_snowprofileLayers <- function(object, silent = FALSE) {

  cols <- colnames(object)
  knownNames <- c(names(snowprofileLayers(validate = FALSE, dropNAs = FALSE)),
                  c("queryLayerIndex", "refLayerIndex"))
  err <- NULL

  ## mandatory fields:
  mandatory_fields <- c("height", "depth")
  try(if (!(all(mandatory_fields %in% cols))) {
    mmlc <- which(!mandatory_fields %in% cols)
    err <- paste(err, paste("Missing mandatory layer characteristics", mandatory_fields[mmlc], "") , sep = "\n ")
  })

  ## unknown fields:
  field_known <- cols %in% knownNames
  if (!all(field_known))
    warning(paste("There are unknown field names in your profile:", paste(cols[!field_known], collapse = ", "),"
                  see ?snowprofileLayers for naming conventions"))

  ## type assertions:
  numeric_vars <- c('height', 'depth', 'thickness', 'temperature', 'density', 'gsize', 'gsize_max', 'gsize_avg', 'ssi')
  for (v in numeric_vars){
    if (v %in% cols)
      try(if (!is.numeric(object[[v]])) err <- paste(err, paste(v, "needs to be numeric"), sep = "\n "))
  }

  factor_vars <- c('gtype', 'gtype_sec')
  for (v in factor_vars){
    if (v %in% cols) {
      try(if (!is.factor(object[[v]])) err <- paste(err, paste(v, "needs to be a factor"), sep = "\n "))
      ## also check for unknown grain types:
      if (v %in% c('gtype', 'gtype_sec')) {
        try(if (((!all(object[[v]] %in% c(grainDict$gtype, NA_character_)) && (!all(is.na(object[[v]]))))))
          err <- paste(err, paste0("unknown ", v, " (s), use simplifyGtypes"), sep = "\n "))
      }
    }
  }

  date_vars <- c('bdate')
  for (v in date_vars){
    if (v %in% cols)
      try(if (!inherits(object[[v]], 'Date')) err <- paste(err, paste(v, "needs to be of class Date"), sep = "\n "))
  }

  posix_vars <- c('ddate')
  for (v in posix_vars){
    if (v %in% cols)
      try(if (!inherits(object[[v]], 'POSIXct')) err <- paste(err, paste(v, "needs to be of class POSIXct"), sep = "\n "))
  }

  if ('hardness' %in% cols)
    try(if (!is.numeric(object$hardness))
      err <- paste(err, "hardness needs to be numeric, use char2numHHI for conversion", sep = "\n "))

  ## assert layer ordering:
  ## -> important for plot, dtw (openEnd), dtw routines (efficiency between tail and max?)
  if (nrow(object) > 1) {
    if ("height" %in% cols)
      try(if (tail(object$height, 1) != max(object$height))
        err <- paste(err, "layer order wrong! last data.frame entry needs to be at snow surface", sep = "\n "))
      try(if (any(diff(object$height) <= 0))
        err <- paste(err, "layers are not sorted in monotonicly increasing height!"))

    if ("depth" %in% cols)
      try(if (tail(object$depth, 1) != min(object$depth))
        err <- paste(err, "layer order wrong! last data.frame entry needs to be at snow surface", sep = "\n "))
  }

  ## raise error
  if (silent) {
    ## either raise error silently and return error message or return NULL
    if (!is.null(err)) return(tryCatch(stop(err), error = function(e) e$message))
    else return(NULL)
  } else {
    ## raise error(s)
    if (!is.null(err)) stop(err)
  }


}


