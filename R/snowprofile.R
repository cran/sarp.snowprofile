## ####################################################

## snowprofile class definition, following guidelines in:
## https://adv-r.hadley.nz/s3.html

#######################################################
## authors: shorton, fherla


#' Check class snowprofile
#'
#' Check if object is of class [snowprofile]
#'
#' @param x object to test
#'
#' @return boolean
#'
#' @export
#'
is.snowprofile <- function(x) inherits(x, "snowprofile")


#' Print snowprofile object
#'
#' Print snowprofile object
#'
#' @param x [snowprofile] object
#' @param pretty pretty print the object (data.frame-like instead of list-like)
#' @param nLayers only print the first few layers (cf., [head])
#' @param ... passed to [print.default]
#'
#' @return object gets printed to console
#'
#' @examples
#'
#' ## pretty print
#' SPpairs$A_manual
#' ## or alternatively:
#' print(SPpairs$A_manual)
#' ## reduce number of layers printed:
#' print(SPpairs$A_manual, nLayers = 6)
#'
#' ## print profile non-pretty (i.e., like the data is stored):
#' print(SPpairs$A_manual, pretty = FALSE)
#'
#' @export
#'
print.snowprofile <- function(x, pretty = TRUE, nLayers = NA, ...) {

  if (pretty) {
    vars <- x
    vars$layers <- NULL
    MetaData <- sapply(vars, function(i) paste(i, collapse = " "))
    print(as.data.frame(MetaData))
    if (is.na(nLayers)) {
      print(x$layers)
    } else {
      print(head(x$layers, nLayers))
      ndiff <- nrow(x$layers) - nLayers
      if (ndiff > 0) print(paste0("  -- ", ndiff, " layers omitted --  "))
    }
  } else {
    print.default(x)
  }
}


#' Low-level constructor function for a snowprofile object
#'
#' Low-cost, efficient constructor function to be used by users who know what they're doing. If that's not you,
#' use the high-level constructor [snowprofile].
#' @param station character string
#' @param station_id character string
#' @param datetime date and time as class POSIXct in most meaningful timezone (timezone can be converted very easily:
#' e.g. `print(profile$datetime, tz = 'EST')` defaults to `'1999-12-31 UTC'`
#' @param latlon 2-element vector latitude (first), longitude (second)
#' @param elev profile elevation (m)
#' @param angle slope angle (degree)
#' @param aspect slope aspect (degree)
#' @param hs total snow height (cm); if not provided, the field will be derived from the profile layers.
#' @param type character string, must be either 'manual', 'vstation' or 'aggregate'
#' @param band character string describing elevation band as ALP, TL, BTL (alpine, treeline, below treeline)
#' @param zone character string describing the zone or region of the profile location (e.g., BURNABY_MTN)
#' @param layers [snowprofileLayers] object
#'
#' @return snowprofile object
#'
new_snowprofile <- function(station = character(),
                            station_id = character(),
                            datetime = as.POSIXct(NA),
                            latlon = as.double(c(NA, NA)),
                            elev = double(),
                            angle = double(),
                            aspect = double(),
                            hs = double(),
                            type = character(),
                            band = character(),
                            zone = character(),
                            layers = snowprofileLayers()) {

  ## check input types: omitted to save resources!

  ## format latlon:
  latlon <- matrix(latlon, nrow = 1, dimnames = list("", c("latitude", "longitude")))

  ## create list and assign class:
  object <- list(station = station,
                 station_id = station_id,
                 datetime = datetime,
                 latlon = latlon,
                 elev = elev,
                 angle = angle,
                 aspect = aspect,
                 hs = hs,
                 type = type,
                 band = band,
                 zone = zone,
                 layers = layers)

  class(object) <- "snowprofile"

  return(object)
}


#' High-level constructor for a snowprofile object
#'
#' Conveniently create a snowprofile object. Calls low-level constructor (only available internally: [new_snowprofile]), asserts correctness
#' through a snowprofile validator function ([validate_snowprofile]) and yields meaningful error messages. Use low-level constructor if you generate many (!) profiles.
#'
#' @param station character string
#' @param station_id character string
#' @param datetime date and time as class POSIXct in most meaningful timezone (timezone can be converted very easily:
#' e.g. `print(profile$datetime, tz = 'EST')`.
#' @param latlon 2-element vector latitude (first), longitude (second)
#' @param elev profile elevation (m)
#' @param angle slope angle (degree)
#' @param aspect slope aspect (degree)
#' @param hs total snow height (cm); if not provided, the field will be derived from the profile layers.
#' @param type character string, must be either 'manual', 'modeled', 'vstation', 'aggregate', or 'whiteboard'
#' @param band character string describing elevation band as ALP, TL, BTL (alpine, treeline, below treeline)
#' @param zone character string describing the zone or region of the profile location (e.g., BURNABY_MTN)
#' @param layers [snowprofileLayers] object
#' @param validate Validate the object with [validate_snowprofile]?
#' @param dropNAs Do you want to drop non-mandatory `snowprofile` and `snowprofileLayers` fields that are `NA` only?
#'
#' @return snowprofile object
#'
#' @author shorton, fherla
#'
#' @seealso [summary.snowprofile], [plot.snowprofile], [snowprofileLayers], [SPpairs]
#'
#' @examples
#'
#' ## Empty snowprofile:
#' snowprofile()
#'
#' ## Test profile:
#' testProfile <- snowprofile(station = 'SARPstation', station_id = 'SARP007',
#'                            datetime = as.POSIXct('2019/04/01 10:00:00', tz = 'PDT'),
#'                            latlon = c(49.277223, -122.915084), aspect = 180,
#'                            layers = snowprofileLayers(height = c(10, 25, 50),
#'                                                       hardness = c(3, 2, 1),
#'                                                       gtype = c('FC', NA, 'PP')))
#' summary(testProfile)
#' plot(testProfile)
#'
#' @export
#'
snowprofile <- function(station = as.character(NA),
                        station_id = as.character(NA),
                        datetime = as.POSIXct(NA, tz = "UTC"),
                        latlon = as.double(c(NA, NA)),
                        elev = as.double(NA),
                        angle = as.double(NA),
                        aspect = as.double(NA),
                        hs = as.double(NA),
                        type = "manual",
                        band = as.character(NA),
                        zone = as.character(NA),
                        layers = snowprofileLayers(formatTarget = "FALSE", dropNAs = FALSE),
                        validate = TRUE,
                        dropNAs = TRUE) {

  ## convert latlon to SpatialPoins if given as pair of coordinates:
  # if (length(latlon) == 2) {
  #   assert_that(is.double(latlon), msg = 'latlon is of length 2, but contains non-numeric type(s)')
  #   latlon <- sp::SpatialPoints(matrix(c(latlon[1], latlon[2]), nrow = 1, dimnames = list('coords', c('longitude', 'latitude'))),
  #                               proj4string = sp::CRS('+proj=longlat +datum=WGS84'))
  # }

  ## format hs:
  if (is.na(hs)) {
    cols_layers <- colnames(layers)
    if ("height" %in% cols_layers) {
      hs <- max(layers$height)
    } else if (all(c("depth", "thickness") %in% cols_layers)) {
      argmax <- which.max(layers$depth)
      hs <- layers$depth[argmax] + layers$thickness[argmax]
    }
  }

  ## type conversion:
  sapply(c(type), function(x) x <- as.character(x))
  sapply(c(latlon, elev, angle, aspect, hs), function(x) x <- as.double(x))

  ## create snowprofile object
  object <- new_snowprofile(station = station,
                            station_id = station_id,
                            datetime = datetime,
                            latlon = latlon,
                            elev = elev,
                            angle = angle,
                            aspect = aspect,
                            hs = hs,
                            type = type,
                            band = band,
                            zone = zone,
                            layers = layers)

  ## add date from datetime
  object$date <- as.Date(object$datetime)

  ## define mandatory fields:
  mandatory_fields <- c("station", "station_id", "datetime", "date", "latlon",
                        "elev", "angle", "aspect", "hs", "type", "layers")

  ## drop NAs and run checks:
  if (dropNAs) {
    object <- c(object[mandatory_fields], object[which(!is.na(object) & !names(object) %in% mandatory_fields)])
    class(object) <- "snowprofile"
  }
  if (validate) validate_snowprofile(object)

  return(object)
}


#' Validate correctness of snowprofile object
#'
#' Validator function that checks if snowprofile standards are being met and raises an error if mandatory fields are
#' missing or data types are incorrect. The function raises a warning when unknown field names are encountered.
#'
#' @param object a [snowprofile] object to be validated
#' @param silent remain silent upon error (i.e., don't raise error, but only print it)
#'
#' @return Per default an error is raised when discovered, if `silent = TRUE` the error is only printed and the
#' error message returned (Note: a warning is never returned but only printed!).
#' If the function is applied to multiple objects, the function returns `NULL` for each object if no error
#' is encountered (see examples below).
#'
#' @examples
#'
#' ## Validate individual snowprofile and raise an error
#' ## in case of a malformatted profile:
#'
#' ## (1) no error
#' validate_snowprofile(SPgroup[[1]])
#'
#' ## (2) malformatted profile --> error
#' this_throws_error <- TRUE
#' if (!this_throws_error) {
#' validate_snowprofile(SPmalformatted[[1]])
#' }
#'
#' ## Validate a list of snowprofiles and raise an error
#' ## when the first error is encountered:
#' ## (i.e., stop subsequent execution)
#'
#' ## (1) no error
#' lapply(SPgroup, validate_snowprofile)
#'
#' ## (2) malformatted profile --> error
#' if (!this_throws_error) {
#' lapply(SPmalformatted, validate_snowprofile)
#' }
#'
#' ## Validate a list of snowprofiles and continue execution,
#' ## so that you get a comprehensive list of errors of all profiles:
#' if (!this_throws_error) {
#' errorlist <- lapply(SPmalformatted, validate_snowprofile, silent = TRUE)
#' }
#'
#' @seealso [reformat_snowprofile]
#' @export
#'
validate_snowprofile <- function(object, silent = FALSE) {

  ## only test class snowprofile objects:
  if (!is.snowprofile(object)) stop("Needs to be a snowprofile object!")

  ## initialize error string:
  err <- NULL
  objNames <- names(object)
  knownNames <- names(snowprofile(validate = FALSE, dropNAs = FALSE))

  ## mandatory fields:
  mandatory_fields <- names(snowprofile(validate = FALSE, dropNAs = TRUE))
  field_check <- mandatory_fields %in% objNames
  if (!all(field_check))
    err <- paste(err, paste("Missing mandatory snowprofile fields:",
                             paste(mandatory_fields[!field_check], collapse = ", ")),
                 sep = "\n ")

  field_known <- objNames %in% knownNames
  if (!all(field_known))
    warning(paste("There are unknown field names in your profile:", paste(objNames[!field_known], collapse = ", "),"
                  see ?snowprofile for naming conventions"))


  ## type assertions:
  try(if (length(object$latlon) != 2 & !is.double(object["latlon"]))
    err <- paste(err, "latlon needs to be numeric and of length 2", sep = "\n "))

  try(if (!inherits(object$datetime, "POSIXct"))
    err <- paste(err, "datetime needs to be of class POSIXct", sep = "\n "))

  try(if (!inherits(object$date, "Date"))
    err <- paste(err, "date needs to be of class Date", sep = "\n "))

  try(if (!(object$type %in% c("manual", "modeled", "aggregate", "vstation", "whiteboard")))
    err <- paste(err, "type must be either manual, modeled, vstation, aggregate, or whiteboard", sep = "\n "))

  for (x in c("elev", "angle", "aspect", "hs")) {
    try(if (!is.numeric(object[[x]]))
      err <- paste(err, paste(x, "needs to be numeric"), sep = "\n "))
  }

  ## validate layers
  err_layers <- validate_snowprofileLayers(object$layers, silent = TRUE)

  ## raise error
  ## (Attention: you have to check for two error strings: err & err_layers!)
  if (silent) {
    ## either raise error silently and return error message or return NULL
    if ((!is.null(err)) | (!is.null(err_layers)))
      return(tryCatch(stop(c(err, err_layers)), error = function(e) e$message))
    else return(NULL)
  } else {
    ## raise error(s)
    if ((!is.null(err)) | (!is.null(err_layers))) stop(c(err, err_layers))
  }

}


#' Reformat a malformatted snowprofile object
#'
#' Reformat a malformatted snowprofile object. A malformatted object may use field names that deviate from our
#' suggested field names (e.g., `grain_type` instead of `gtype`), or it may use data types that are different than
#' what we suggest to use (e.g., `ddate` as type `Date` instead of `POSIXct`). Basically, if your snowprofile object
#' fails the test of [validate_snowprofile] due to the above reason this function should fix it.
#'
#' @param profile [snowprofile] object
#' @param currentFields array of character strings specifying the current field names that you want to change
#' @param targetFields array of same size than `currentFields` specifying the new field names
#'
#' @examples
#'
#' ## check the malformatted profile:
#' this_throws_error <- TRUE
#' if (!this_throws_error) {
#' validate_snowprofile(SPmalformatted[[1]])
#' }
#' ## i.e., we see that elev and ddate are of wrong data type,
#' ## and a warning that grain_type is an unknown layer property.
#'
#' ## reformat field types, but not the field name:
#' betterProfile <- reformat_snowprofile(SPmalformatted[[1]])
#' ## i.e., no error is raised anymore, but only the grain_type warning
#'
#' ## so let's reformat also the field names:
#' optimalProfile <- reformat_snowprofile(SPmalformatted[[1]], "grain_type", "gtype")
#'
#'
#'
#' ## reformat a list of profiles with the same configuration:
#' SPmalformatted_reformatted <- lapply(SPmalformatted, reformat_snowprofile,
#'                                      currentFields = "grain_type", targetFields = "gtype")
#'
#' ## the malformatted profile set finally is correctly formatted:
#' lapply(SPmalformatted_reformatted, validate_snowprofile)
#'
#'
#' @export
#'
reformat_snowprofile <- function(profile, currentFields = NULL, targetFields = NULL) {

  ## ----assertions and initialization----
  if (!is.snowprofile(profile)) stop("Not a class snowprofile object")
  if (length(currentFields) != length(targetFields)) stop("currentFields and targetFields need to be of same length!")

  emptyPro <- snowprofile()
  metaNames <- names(emptyPro)[names(emptyPro) != "layers"]
  metaTypes <- sapply(emptyPro[metaNames], class)
  layerNames <- names(emptyPro$layers)
  layerTypes <- sapply(emptyPro$layers[layerNames], class)
  mlNames <- c(metaNames, layerNames)
  mlTypes <- c(metaTypes, layerTypes)

  ## ----profile meta----
  metaIdx <- which(targetFields %in% metaNames)
  profile[targetFields[metaIdx]] <- profile[currentFields[metaIdx]]
  profile[currentFields[metaIdx]] <- NULL


  ## ----profile layers----
  layerIdx <- which(targetFields %in% layerNames)
  profile$layers[targetFields[layerIdx]] <- profile$layers[currentFields[layerIdx]]
  profile$layers[currentFields[layerIdx]] <- NULL

  ## ----type conversion----
  ## check which field types yield errors:
  msg <- tryCatch({validate_snowprofile(profile)},
                   error = function(e) e$message)
  if (length(msg) > 0) {
    msgSplit <- strsplit(msg, split = " ")[[1]]
    metaConvert <- msgSplit[sapply(msgSplit, `%in%`, metaNames)]
    layerConvert <- msgSplit[sapply(msgSplit, `%in%`, layerNames)]

    ## convert those field types according to types in snowprofile() constructor function
    if (length(metaConvert) > 0) {
      if (length(profile$latlon) != 2) stop("latlon needs to be a vector of length 2!")
      if ("date" %in% metaConvert) try(profile$date <- as.Date(profile$datetime))
      for (mconv in metaConvert) {
        profile[mconv] <- do.call(paste0("as.", metaTypes[mconv][[1]][[1]]), unname(profile[mconv]))
      }
    }

    if (length(layerConvert) > 0) {
      for (lconv in layerConvert) {
        profile$layers[lconv] <- do.call(paste0("as.", layerTypes[lconv][[1]][[1]]), unname(profile$layers[lconv]))
      }
    }
  }

  return(profile)
}

