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


#' Low-level constructor for a snowprofileLayers object
#'
#' Low-cost, efficient constructor function to be used by users who know what they're doing. \cr \cr
#' \strong{Important:} Make sure the last row of the data.frame corresponds to the snow surface. No checks incorporated for this low-level constructor!
#'
#' @param ... see [snowprofileLayers]
#'
#' @return snowprofileLayers object as data.frame with strings as factors
#'
#' @export
#'
new_snowprofileLayers <- function(...) {

  entries <- list(...)
  layers <- as.data.frame(entries, stringsAsFactors = TRUE)
  class(layers) <- append("snowprofileLayers", class(layers))

  return(layers)
}


#' High-level constructor for a snowprofileLayers object
#'
#' Helper function to conveniently create a snowprofileLayers object, i.e. data.frame with mandatory column fields height (or depth) that provides vertical position of layers.
#' Layers need to be ordered in an ascending manner, i.e. last row corresponds to snow surface. If only depth is given, the layer thickness of the lowermost
#' layer will be set to a default value (100 cm) to be able to convert to height (i.e. important for subsequent package routines). If the columns are not of equal
#' lengths, their values will be recycled (default data.frame mechanism), but a warning will be issued. Certain columns will be auto-filled
#' ([format_snowprofileLayers]). Instead of individual layer characteristics, a data.frame can be provided, which will be converted into a snowprofileLayers class.
#' Calls low-level constructor [new_snowprofileLayers] and asserts correctness through a call to [validate_snowprofileLayers].
#'
#' @param height height vector (cm)
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
#' @param ssi snow stability index (numeric)
#' @param ... columns to include in the layers object. Note, that they need to correspond to the according height/depth array.
#' e.g. hardness (can use character hardness or numeric hardness via [char2numHHI]), ddate (class POSIX), bdate (class Date) gtype (character or factor), density, temperature, gsize, lwc, gsize_max, gtype_sec, ssi, depth, thickness
#' @param hs total snow height (cm), if not deductible from height or depth & thickness vector
#' @param formatTarget string indicating which layer characteristics should be auto-filled, e.g. 'all' (default), 'height', 'depth', 'thickness', 'none'
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
#' snowprofileLayers(dropNAs = FALSE)
#'
#'
#' ## convert and recycle character hardness (i.e., warning issued):
#' snowprofileLayers(height = c(10, 25, 50),
#'                   hardness = char2numHHI('1F+'),
#'                   gtype = c('FC', NA, 'PP'))
#'
#'
#' ## create snowprofileLayers object from pre-existant data.frame:
#' df <- data.frame(height = c(10, 25, 50),
#'                   hardness = c(2, 3, 1),
#'                   gtype = c('FC', NA, 'PP'),
#'                   stringsAsFactors = TRUE)
#'
#' snowprofileLayers(layerFrame = df)
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
                              ssi = as.double(NA),
                              ...,
                              hs = as.double(NA),
                              formatTarget = "all",
                              layerFrame = NA,
                              validate = TRUE,
                              dropNAs = TRUE) {

  ## warn if recycling values
  entries <- list(height = height, ...)
  if (any(diff(sapply(entries, length)) != 0)) warning("not all inputs have the same length, recycling..")

  ## create snowprofile object either from specific input variables or from layerFrame:
  if (all(is.na(entries)) && is.data.frame(layerFrame)) {
    object <- new_snowprofileLayers(layerFrame)
  } else {
    object <- new_snowprofileLayers(height = height,
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
                                    ssi = ssi,
                                    ...)
  }

  ## run checks and format layers:
  object <- format_snowprofileLayers(object, target = formatTarget, hs = hs, validate = validate, dropNAs = dropNAs)

  return(object)
}


#' Format snowprofileLayers
#'
#' Calculate missing data.frame columns based on the given ones, if possible.
#'
#' @param obj snowprofileLayers object
#' @param target string, indicating which fields are auto-filled ('all', 'height', 'depth', 'thickness', 'none')
#' @param hs total snow height (cm) if not deductible from given fields
#' @param validate Validate `obj` with [validate_snowprofileLayers]?
#' @param dropNAs Do you want to drop all columns consisting of NAs only?
#'
#' @return copy of obj with auto-filled columns
#'
#' @export
#'
format_snowprofileLayers <- function(obj, target = "all", hs = NA, validate = TRUE, dropNAs = TRUE) {

  ## initial assertions: validate input
  if (validate) validate_snowprofileLayers(obj)

  ## drop all columns that contain only NA values:
  if (dropNAs) object <- obj[, colSums(is.na(obj)) < nrow(obj)]
  else object <- obj
  cols <- colnames(object)
  hsmin <- ifelse("height" %in% cols, tail(object$height, 1), object$depth[1])
  if (all(!is.na(c(hs, hsmin)))) try(if (hs < hsmin) stop(paste("'hs' must be >=", hsmin, "cm")))

  ## decide which columns to calculate:
  if (target == "all") target <- c("height", "depth", "thickness")
  do <- target[!target %in% cols]

  ## do conversion:
  if (any(do %in% "height")) {
    ## calculate hs:
    if (is.na(hs)) {
      if (c("thickness") %in% cols) {
        hs <- object$depth[1] + object$thickness[1]
      } else {
        warning("total snow height 'hs' is unknown, setting lowest layer thickness to 100 cm")
        hs <- object$depth[1] + 100
      }
    }
    object$height <- hs - object$depth
  }

  if (any(do %in% "depth")) {
    hs <- max(object$height)
    object$depth <- hs - object$height
  }

  if (any(do %in% "thickness")) {
    if (exists("height", where = object)) {
      object$thickness <- diff(c(0, object$height))
    } else {
      if (is.na(hs)) {
      warning("total snow height 'hs' is unknown, setting lowest layer thickness to 100 cm")
      hs <- object$depth[1] + 100
      }
      object$thickness <- diff(c(hs, object$depth))
    }
  }

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
  knownNames <- names(snowprofileLayers(validate = FALSE, dropNAs = FALSE))
  err <- NULL

  ## mandatory fields:
  try(if (!(any(c("height", "depth") %in% cols)))
    err <- paste(err, "Missing mandatory layer characteristics 'height' or 'depth'", sep = "\n "))

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
    if (v %in% cols)
      try(if (!is.factor(object[[v]])) err <- paste(err, paste(v, "needs to be a factor"), sep = "\n "))
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


