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
    if ("temperatureProfile" %in% names(vars)) vars$temperatureProfile <- NULL
    if ("tests" %in% names(vars)) vars$tests <- NULL
    if ("instabilitySigns" %in% names(vars)) vars$instabilitySigns <- NULL
    if ("backtrackingTable" %in% names(vars)) vars$backtrackingTable <- "available, but omitted here"
    if ("sim2group" %in% names(vars)) vars$sim2group <- "available, but omitted here"
    MetaData <- sapply(vars, function(i) paste(i, collapse = " "))
    print(as.data.frame(MetaData))
    if (is.na(nLayers)) {
      cat("layers\n")
      print(x$layers)
      if ("temperatureProfile" %in% names(x)) {
        cat("temperatureProfile\n")
        print(x$temperatureProfile)
      }
      if ("tests" %in% names(x)) {
        cat("tests\n")
        print(x$tests)
      }
      if ("instabilitySigns" %in% names(x)) {
        cat("instabilitySigns\n")
        print(x$instabilitySigns)
      }
    } else {
      print(head(x$layers, nLayers))
      ndiff <- nrow(x$layers) - nLayers
      if (ndiff > 0) print(paste0("  -- ", ndiff, " layers omitted --  "))
      if ("temperatureProfile" %in% names(x)) {
        cat("temperatureProfile\n")
        print(head(x$temperatureProfile, nLayers))
      }
      if ("tests" %in% names(x)) {
        cat("tests\n")
        print(x$tests)
      }
      if ("instabilitySigns" %in% names(x)) {
        cat("instabilitySigns\n")
        print(x$instabilitySigns)
      }
    }
  } else {
    print.default(x)
  }
}


#' Low-level constructor function for a snowprofile object
#'
#' Low-cost, efficient constructor function to be used by users who know what they're doing. If that's not you,
#' use the high-level constructor [snowprofile].
#' @inheritParams snowprofile
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
                            maxObservedDepth = double(),
                            type = character(),
                            band = character(),
                            zone = character(),
                            comment = character(),
                            hn24 = double(),
                            hn72 = double(),
                            ski_pen = double(),
                            layers = snowprofileLayers(),
                            tests = snowprofileTests(),
                            instabilitySigns = snowprofileInstabilitySigns()) {

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
                 maxObservedDepth = maxObservedDepth,
                 type = type,
                 band = band,
                 zone = zone,
                 comment = comment,
                 hn24 = hn24,
                 hn72 = hn72,
                 ski_pen = ski_pen,
                 layers = layers,
                 tests = tests,
                 instabilitySigns = instabilitySigns)

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
#' @param maxObservedDepth equivalent to `hs` for full profiles that go down to the ground. for test profiles that only
#' observe the upper part of the snowpack this value refers to the maximum depth of the profile observation.
#' @param type character string, must be either 'manual', 'modeled', 'vstation', 'aggregate', or 'whiteboard'
#' @param band character string describing elevation band as ALP, TL, BTL (alpine, treeline, below treeline)
#' @param zone character string describing the zone or region of the profile location (e.g., BURNABY_MTN)
#' @param comment character string with any text comments
#' @param hn24 height of new snow within 24 h
#' @param hn72 height of new snow within 72 h
#' @param ski_pen skier penetration depth (cm)
#' @param layers [snowprofileLayers] object
#' @param tests [snowprofileTests] object
#' @param instabilitySigns [snowprofileInstabilitySigns] object
#' @param validate Validate the object with [validate_snowprofile]?
#' @param dropNAs Do you want to drop non-mandatory `snowprofile` and `snowprofileLayers` fields that are `NA` only?
#'
#' @return snowprofile object
#'
#' @author shorton, fherla
#'
#' @seealso [summary.snowprofile], [plot.snowprofile], [snowprofileLayers], [snowprofileTests], [snowprofileInstabilitySigns], [SPpairs]
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
                        datetime = as.POSIXct(NA),
                        latlon = as.double(c(NA, NA)),
                        elev = as.double(NA),
                        angle = as.double(NA),
                        aspect = as.double(NA),
                        hs = as.double(NA),
                        maxObservedDepth = as.double(NA),
                        type = "manual",
                        band = as.character(NA),
                        zone = as.character(NA),
                        comment = as.character(NA),
                        hn24 = as.double(NA),
                        hn72 = as.double(NA),
                        ski_pen = as.double(NA),
                        layers = snowprofileLayers(dropNAs = FALSE, validate = FALSE),
                        tests = snowprofileTests(dropNAs = FALSE),
                        instabilitySigns = snowprofileInstabilitySigns(dropNAs = FALSE),
                        validate = TRUE,
                        dropNAs = TRUE) {

  ## convert latlon to SpatialPoins if given as pair of coordinates:
  # if (length(latlon) == 2) {
  #   assert_that(is.double(latlon), msg = 'latlon is of length 2, but contains non-numeric type(s)')
  #   latlon <- sp::SpatialPoints(matrix(c(latlon[1], latlon[2]), nrow = 1, dimnames = list('coords', c('longitude', 'latitude'))),
  #                               proj4string = sp::CRS('+proj=longlat +datum=WGS84'))
  # }

  ## Check and format hs and maxObservedDepth:
  cols_layers <- colnames(layers)
  vloc_max <- suppressWarnings( max(c(ifelse("height" %in% cols_layers, max(layers$height), NA),
                                      ifelse(all(c("depth", "thickness") %in% cols_layers), layers$depth[1] + layers$thickness[1], NA)),
                                    na.rm = TRUE) )
  vloc_max <- ifelse(is.infinite(vloc_max), as.double(NA), vloc_max)
  ## query for basal layer thickness NA --> hs was unknown when snowprofileLayers object was created!
  if (is.na(layers$thickness[1])) {
    maxObservedDepth_from_layers <- layers$depth[1]
    hs_from_layers <- as.double(NA)
  } else if (hasUnobservedBasalLayer(layers)) {
    ## in this case, hs was known, but basal layer(s) not observed down to ground!
    maxObservedDepth_from_layers <- layers$depth[1]
    hs_from_layers <- vloc_max
  } else {
    maxObservedDepth_from_layers <- vloc_max
    hs_from_layers <- vloc_max
  }

  if (is.na(hs)) {
    hs <- hs_from_layers
  } else {
    if (!isTRUE(all.equal(hs, hs_from_layers))) {
      hs_new <- max(c(hs, hs_from_layers), na.rm = TRUE)
      warning(paste0("hs (", hs, " cm) is different than hs deducted from your provided layers (", hs_from_layers, " cm)\n",
                     "--> Setting hs to ", hs_new, " cm."))
      hs <- hs_new
    }
  }
  if (is.na(maxObservedDepth)) {
    maxObservedDepth <- maxObservedDepth_from_layers
  } else {
    if (!isTRUE(all.equal(maxObservedDepth, maxObservedDepth_from_layers))) {
      maxObservedDepth_new <- max(c(maxObservedDepth, maxObservedDepth_from_layers), na.rm = TRUE)
      warning(paste0("maxObservedDepth (", maxObservedDepth, " cm) is different than maxObservedDepth deducted from your provided layers (", maxObservedDepth_from_layers, " cm)\n",
                     "--> Setting maxObservedDepth to ", maxObservedDepth_new, " cm."))
      maxObservedDepth <- maxObservedDepth_new
    }
  }

  ## type conversion:  NOTE: the two lines are uncommented b/c the code doesn't actually convert the type. Left to be done!
  # sapply(c(type), function(x) x <- as.character(x))
  # sapply(c(latlon, elev, angle, aspect, hs, maxObservedDepth), function(x) x <- as.double(x))

  ## create snowprofile object
  object <- new_snowprofile(station = station,
                            station_id = station_id,
                            datetime = datetime,
                            latlon = latlon,
                            elev = elev,
                            angle = angle,
                            aspect = aspect,
                            hs = hs,
                            maxObservedDepth = maxObservedDepth,
                            type = type,
                            band = band,
                            zone = zone,
                            comment = comment,
                            hn24 = hn24,
                            hn72 = hn72,
                            ski_pen = ski_pen,
                            layers = layers,
                            tests = tests,
                            instabilitySigns = instabilitySigns)

  ## add date from datetime
  object$date <- as.Date(as.character(object$datetime))  # double conversion ensures that timezone won't change from POSIXct to Date format!

  ## define mandatory fields:
  ## IMPORTANT: needs to be synced manually with mandatory_fields defined in snowprofile validator!!
  mandatory_fields <- c("station", "station_id", "datetime", "date", "latlon",
                        "elev", "angle", "aspect", "hs", "maxObservedDepth", "type", "layers")

  ## drop NAs and run checks:
  if (dropNAs) {
    toKeep <- unique(c(mandatory_fields, names(which(!(sapply(object, function(x) (all(is.na(x)) || isTRUE(all(dim(x)[[1]] == 0))) ))))))
    object <- object[toKeep]
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
#' errorlist[sapply(errorlist, function(item) !is.null(item))]  # print profiles that caused errors
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
  objNames <- objNames[order(objNames)]  # sort alphabetically
  knownNames <- c(names(snowprofile(validate = FALSE, dropNAs = FALSE)), "psum", "psum24")
  knownNames <- knownNames[order(knownNames)]  # sort alphabetically


  ## mandatory fields:  IMPORTANT: needs to be synced manually with mandatory_fields defined in snowprofile constructor!!
  mandatory_fields <- mandatory_fields <- c("station", "station_id", "datetime", "date", "latlon",
                                            "elev", "angle", "aspect", "hs", "maxObservedDepth", "type", "layers")
  mandatory_fields <- mandatory_fields[order(mandatory_fields)]  # sort alphabetically
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

  for (x in c("elev", "angle", "aspect", "hs", "maxObservedDepth")) {
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

  emptyPro <- snowprofile(validate = FALSE, dropNAs = FALSE)
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

    if ("instabilitySigns" %in% metaConvert) stop("$instabilitySigns contain errors, fix manually!")
    if ("tests" %in% metaConvert) stop("$tests contain errors, fix manually!")

    ## convert those field types according to types in snowprofile() constructor function
    if (length(metaConvert) > 0) {
      if (length(profile$latlon) != 2) stop("latlon needs to be a vector of length 2!")
      if ("date" %in% metaConvert) try(profile$date <- as.Date(as.character(profile$datetime)))
      for (mconv in metaConvert) {
        profile[mconv] <- do.call(paste0("as.", metaTypes[mconv][[1]][[1]]), unname(profile[mconv]))
      }
    }

    if (length(layerConvert) > 0) {
      for (lconv in layerConvert) {
        if (lconv %in% "gtype") {
          ## if gtype doesn't exist --> create it with NAs
          if (!"gtype" %in% names(profile$layers)) profile$layers$gtype <- as.factor(rep(as.character(NA), times = nrow(profile$layers)))
        }
        profile$layers[lconv] <- do.call(paste0("as.", layerTypes[lconv][[1]][[1]]), unname(profile$layers[lconv]))
        if (lconv %in% c("gtype","gtype_sec")){
          unknown_gtypes <- which(!profile$layers[[lconv]] %in% c(grainDict$gtype, NA_character_))
          if (length(unknown_gtypes > 0)) warning("There are be unknown grain types in your profile...simplifying them to their parent classes!")
          converted_gtypes <- substr(as.character(profile$layers[[lconv]][unknown_gtypes]), 1, 2)
          profile$layers[lconv] <- as.character(profile$layers[[lconv]])
          profile$layers[[lconv]][unknown_gtypes] <- converted_gtypes
          profile$layers[[lconv]] <- as.factor(profile$layers[[lconv]])
        }
      }
    }
  }

  return(profile)
}

