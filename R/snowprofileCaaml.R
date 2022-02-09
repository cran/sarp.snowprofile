#' Read a Caaml file into a snowprofile object
#'
#' Note, that this function only provides a starting point for loading caaml files into R. Currently, caaml files exported from
#' niviz.org, or snowpilot.org should be compatible with this routine. However, this routine only extracts some metadata
#' and some of the most important layer characteristics. While a temperature profile (that is independent from the layers)
#' is extracted, no other variables that can be written into a caaml file are currently being read (such as stability test results, etc).
#'
#' * There is still a bug related to non-numeric aspects (e.g., E instead of 90).
#' * The [snowprofileCsv] function provides a lot more flexibility to read in data, if you can choose the format of your
#' underlying data. Don't hesitate to reach out though if your caaml files throw errors and you need help! If you extend this
#' routine, please also reach out and let us know, so we can update this package with your code extensions.
#'
#' @import data.table xml2
#'
#' @param caamlFile 'path/to/file.caaml'
#' @param sourceType choose 'manual', 'modeled', 'vstation', 'aggregate' or 'whiteboard'; while this routine has some
#' functionality built in to detect sourceTypes under certain circumstances, it needs to be provided in most cases.
#' @param readStabilityTests boolean (this is still beta version and can throw errors sometimes)
#' @param validate Should the resulting snowprofile object be validated by [validate_snowprofile]?
#'
#' @return snowprofile object
#'
#' @author fherla
#'
#' @examples
#'
#' ## load example caaml file that ships with package:
#' caamlFile <- system.file('extdata', 'example.caaml', package = 'sarp.snowprofile')
#'
#' ## read caaml file:
#' profile <- snowprofileCaaml(caamlFile, sourceType = 'vstation')
#'
#' ## other file with slighlty different xml namespace, structure, etc (including stability test):
#' caamlFile2 <- system.file('extdata', 'example2.caaml', package = 'sarp.snowprofile')
#' profile2 <- snowprofileCaaml(caamlFile2, sourceType = 'manual')
#'
#' @export
snowprofileCaaml <- function(caamlFile,
                             sourceType = NA,
                             readStabilityTests = TRUE,
                             validate = TRUE) {

  ## --- Initialization, assertions, subfunctions ----

  extractLayerCharacteristics <- function(layer_element, prefix_plain, sourceSoftware) {
    ## extracts most important layer characteristics of one given SnowProfile layer
    ## @param layer_element xml_nodeset of SnowProfile layer
    ## @return data frame containing either double prec float or string, as appropriate

    ## 'lwc' or 'wetness':
    wTry <- xml_text(xml_child(layer_element, prefix_plain("wetness")))
    lwcTry <- xml_text(xml_child(layer_element, prefix_plain("lwc")))
    lwc_write <- ifelse(all(is.na(lwcTry)), yes = wTry, no = lwcTry)

    ## split layer comments into fields:
    lay_comment <- xml_text(xml_child(xml_find_first(layer_element, prefix_plain("metaData")), prefix_plain("comment")))
    fields <- strsplit(lay_comment, " ")[[1]]
    bdate <- fields[grep("....-..-..", fields)]
    if (length(bdate) == 0) bdate <- NA
    else bdate <- as.POSIXct(bdate)
    shovelTest <- fields[grep("ST", fields)]
    if (length(shovelTest) == 0) ST <- NA
    else ST <- as.numeric(strsplit(shovelTest, "ST")[[1]][2])

    ## gsize dependent on sourceSoftware
    if (sourceSoftware == "snowpilot") gs_max_notation <- "avgMax"
    else gs_max_notation <- "max"

    ## return data.frame (those columns that actually contain values)
    layerFrame <- data.frame(depth = xml_double(xml_child(layer_element, prefix_plain("depthTop"))),
                             thickness = xml_double(xml_child(layer_element, prefix_plain("thickness"))),
                             hardness = char2numHHI(xml_text(xml_child(layer_element, prefix_plain("hardness")))),
                             gtype = as.character(xml_text(xml_child(layer_element, prefix_plain("grainFormPrimary")))),
                             gtype_sec = xml_text(xml_child(layer_element, prefix_plain("grainFormSecondary"))),
                             gsize = xml_double(xml_child(xml_child(xml_child(
                               layer_element, prefix_plain("grainSize")), prefix_plain("Components")), prefix_plain("avg"))),
                             gsize_max = xml_double(xml_child(xml_child(xml_child(
                               layer_element, prefix_plain("grainSize")), prefix_plain("Components")), prefix_plain(gs_max_notation))),
                             bdate = bdate,
                             ST = ST,
                             lwc = lwc_write,
                             comment = lay_comment,
                             stringsAsFactors = TRUE)
    layerFrame <- Filter(function(x) !all(is.na(x)), layerFrame)
    return(layerFrame)
  }

  extractTempProfile <- function(layer_element, prefix_plain) {
    ## extracts depth and temperature of one given SnowProfile 'tempProfile' layer and returns data.frame, analogously to above
    tFrame <- data.frame(depth = xml_double(xml_child(layer_element, prefix_plain("depth"))),
                         temperature = xml_double(xml_child(layer_element, prefix_plain("snowTemp"))))
    tFrame <- Filter(function(x) !all(is.na(x)), tFrame)
    return(tFrame)
  }

  extractStabilityTests <- function(layer_element, prefix_plain) {
    ## extracts characteristics of 'stbTests' elements and returns data.frame, analogously to above
    testType_camel <- xml_name(layer_element)
    testType <- gsub("[^A-Z]","", testType_camel)
    # failure layer depth:
    failureLayer <- xml_child(xml_child(layer_element, prefix_plain("failedOn")), prefix_plain("Layer"))
    # initializations
    failureLayerDepth <- NA
    failureLayerComment <- as.character(NA)
    result <- as.character(NA)
    score <- as.double(NA)
    if (length(failureLayer) == 1) {
      failureLayerDepth <- xml_double(failureLayer)
    } else {
      failureLayerDepth <- xml_double(xml_child(failureLayer, prefix_plain("depthTop")))
      failureLayerComment <- xml_text(xml_child(failureLayer, prefix_plain("metaData")))
    }
    # scores as numeric for CTs, but mixed for ECTs (e.g., ECTN20):
    score_str <- xml_text(xml_child(xml_child(xml_child(layer_element, prefix_plain("failedOn")),
                                                prefix_plain("Results")), prefix_plain("testScore")))
    score_dbl <- as.double(regmatches(score_str, regexpr("[[:digit:]]+", score_str)))
    if (length(score_dbl) > 0) score <- score_dbl
    # handle "no failure" test results:
    if (is.na(score_str)) {
      if (isTRUE(xml_name(xml_child(layer_element, prefix_plain("noFailure"))) == "noFailure")) {
        score_dbl <- as.double(NA)
        result <- "N"
        failureLayerDepth <- as.double(NA)
      }
    }

    ## extract result from code (e.g., "P", "N", "M-H", ...)
    ## extract propagation propensity from ECT scores:
    # if (testType == "ECT") {  # deprecated, see two lines below!
    #   result <- ifelse("N" %in% strsplit(score_str, split = "")[[1]], "N", "P")
    # }
    result_split <- strsplit(score_str, split = testType)[[1]]  # yields code without test type, e.g. [ECT]PV, [CT]M-H21
    result_split <- result_split[[length(result_split)]]  # ensure to get the last (i.e., meaningful) entry if multiple exist
    result <- regmatches(result_split, regexpr("[^0-9]+", result_split))  # discards any digits in result_split
    if (length(result) == 0) result <- as.character(NA)  # ensures NA if only digits were available in previous line

    # extract fracture character if available:
    fractChar <- tryCatch({xml_text(xml_child(xml_child(xml_child(layer_element, prefix_plain("failedOn")),
                                                        prefix_plain("Results")), prefix_plain("fractureCharacter")))},
                          error = as.character(NA))
    # construct data frame
    stbFrame <- data.frame(type = testType,
                           result = result,
                           score = score,
                           fract_char = fractChar,
                           depth = failureLayerDepth,
                           comment = failureLayerComment
                          )
    ## drop NA columns -- obsolete since snowprofileTests class exists
    # stbFrame <- Filter(function(x) !all(is.na(x)), stbFrame)
    return(stbFrame)
  }

  ## read caaml file and assert that it's '.caaml' and 'SnowProfile':
  obj <- read_xml(caamlFile)

  splittedFN <- strsplit(tolower(caamlFile), split = "\\.")[[1]]
  if (!try(splittedFN[length(splittedFN)]) == "caaml") warning("Not a .caaml file")
  if (try(xml_name(obj)) != "SnowProfile") stop(paste0("Content of '", caamlFile,
                                                      "' doesn't seem to be a 'SnowProfile'"))

  ## strip namespace from xml-object if it's from SnowPilot.org
  #  and name node-identifiers accordingly
  sourceSoftware <- "NA"  # deliberately use a string here!
  if ("snowpilot" %in% names(xml_ns(obj))) sourceSoftware <- "snowpilot"
  if (isTRUE(sourceSoftware == "snowpilot")) {
    xml_ns_strip(obj)
  ## Explanation: Niviz exports its caaml files with a caaml namespace, so individual nodes are
  # named e.g. 'caaml:timePosition'. On the other hand SnowPilot exports without such a namespace prefix.
  # The package xml2 theoretically provides a functional solution to that problem, but (guess what)
  # I can't get it to work. The workaround is to manually prepend the correct prefix.
    prefix_slash <- function(st) paste0(".//", st)
    prefix_plain <- function(st) st
  } else {
    prefix_slash <- function(st) paste0(".//caaml:", st)
    prefix_plain <- function(st) paste0("caaml:", st)
  }

  ## Extract and write meta-data:
  ## ---Time ----
  datetimeRAW <- gsub("T", " ", as.character(xml_text(xml_find_all(obj, prefix_slash("timePosition")))))
  datetime <- as.POSIXct(substr(datetimeRAW, 1, 23), tz = "UTC")


  ## ---Source and comments----
  observer <- paste(xml_text(xml_contents(xml_contents(xml_find_all(obj, prefix_slash("srcRef"))))), collapse = "|")
  try({com <- xml_text(xml_child(xml_find_first(obj, prefix_plain("metaData")), prefix_plain("comment")))
       if (!is.na(com)) comment_general <- com}, silent = TRUE)
  try({com_loc <- xml_text(xml_child(xml_child(xml_find_all(obj, prefix_slash("locRef")), prefix_plain("metaData")), prefix_plain("comment")))
       if (!is.na(com_loc)) comment_location <- com_loc}, silent = TRUE)

  ## ---Location----
  ## maybe still naming problems with some software?
  ## include here: parent locRef and child ObsPoint
  locRef <- xml_find_all(obj, prefix_slash("locRef"))
  locRefName <- xml_child(locRef, prefix_plain("name"))
  if (is.na(locRefName)) {
    station <- xml_text(xml_child(xml_child(xml_find_all(obj, prefix_slash("locRef")), prefix_plain("ObsPoint"))), prefix_plain("name"))
    station_id <- xml_attr(xml_child(xml_find_all(obj, prefix_slash("locRef")), prefix_plain("ObsPoint")), "id")
  } else {
    station <- xml_text(locRefName)
    station_id <- xml_attr(locRef, "id")
  }

  ## ---Elevation, aspect, angle, latlon----
  elevation <- xml_double(xml_child(xml_find_all(locRef, prefix_slash("ElevationPosition")), prefix_plain("position")))
  if (length(elevation) == 0) elevation <- as.numeric(NA)
  angle <- xml_double(xml_child(xml_find_all(locRef, prefix_slash("SlopeAnglePosition")), prefix_plain("position")))
  if (length(angle) == 0) angle <- as.numeric(NA)
  aspect_node <- xml_child(xml_find_all(locRef, prefix_slash("AspectPosition")), prefix_plain("position"))
  aspect <- tryCatch({xml_double(aspect_node)}, warning = function(w) char2numAspect(xml_text(aspect_node)))
  if (length(aspect) == 0) aspect <- as.numeric(NA)
  lonlat <- strsplit(xml_text(xml_find_all(obj, ".//gml:pos")), split = " ")
  if (length(lonlat) == 0) {
    lat <- as.numeric(NA)
    lon <- as.numeric(NA)
  } else {
    lat <- as.double(lonlat[[1]][2])
    lon <- as.double(lonlat[[1]][1])
    # snowpilot v6 uses latlon instead of lonlat:
    if (isTRUE(sourceSoftware == "snowpilot")){
      lat <- as.double(lonlat[[1]][1])
      lon <- as.double(lonlat[[1]][2])
    }
  }

  ## ---profile Depth or SnowHeight, HST----
  hS <- xml_find_all(obj, prefix_slash("hS"))
  ## sometimes, hS can contain a 'components' nodeset with the actual hS stored as a child height:
  heightTotal <- tryCatch(xml_double(hS),
                          warning = function(w) {
                            choices <- c(xml_double(xml_child(hS, prefix_slash("height"))),
                                         xml_double(xml_child(hS, prefix_slash("snowHeight"))))
                            choices[!is.na(choices)]})

  pD <- xml_double(xml_find_all(obj, prefix_slash("profileDepth")))
  maxObservedDepth <- as.double(NA)
  if (length(pD) == 1) {
    if (!isTRUE(all.equal(heightTotal, pD))) maxObservedDepth <- pD
  }

  ## snowpack conditions (i.a. HST):
  try({spackCondMeta_node <- xml_child(xml_find_first(obj, prefix_slash("snowPackCond")), prefix_plain("metaData"))
      if (!is.na(spackCondMeta_node)) {
        comment_spack <- xml_text(xml_child(spackCondMeta_node), prefix_plain("comment"))
        com_HST_check <- grep("HST[0-9]{1,3}", comment_spack)  # returns index if pattern 'HST###' exists
        if (length(com_HST_check) != 0) {
          com_HST <- gsub(".*(HST[0-9]{1,3}).*", "\\1", comment_spack)  # returns 'HST###'
          HST <- as.numeric(strsplit(com_HST, "HST")[[1]][2])
        }
      }
    }, silent = TRUE)

  ## ---Snow layers----
  ## Extract layers, arange in data.frame (subfunction) and write to snowprofile object:
  layers_snow <- xml_children(xml_find_all(obj, prefix_slash("stratProfile")))
  layerFrame <- data.table::rbindlist(lapply(layers_snow, extractLayerCharacteristics, prefix_plain = prefix_plain,
                                             sourceSoftware = sourceSoftware), fill = TRUE)  # yields data.table
  ## This function was originally designed for 'top down' caaml files (e.g., niViz)
  #  However, snowpilot v5 reports the vertical axis bottom up, therefore depth = height:
  measurement_dir <- xml_attr(xml_find_all(obj, prefix_slash("SnowProfileMeasurements")), "dir")
  if (measurement_dir == "bottom up")  {  # e.g., snowpilot caaml v5
    colnames(layerFrame)[colnames(layerFrame) == "depth"] <- "height"
    layerFrame$depth <- rev(layerFrame$height)
  }
  ## assure that last row of data.table is at the snow surface:
  layerFrame <- layerFrame[order(-layerFrame$depth)]

  ## the following could also be done in snowprofile constructor function:
  ## calculate height if heightTotal is known:
  if (exists("heightTotal")) {
    layerFrame$height <- heightTotal - layerFrame$depth # i.e. height bottom-up at top layer interface
    ## change order of columns in data.table:
    idcols <- c("depth", "thickness", "height")
    colOrder <- c(idcols, names(layerFrame)[-which(names(layerFrame) %in% idcols)])
    setcolorder(layerFrame, neworder = colOrder)
  }

  ## stratigraphy metadata, such as Layer of Concern
  loc_node <- character()
  try({loc_node <- suppressWarnings(xml_find_all(layers_snow, ".//snowpilot:layerOfConcern"))}, silent = TRUE)
  if (length(loc_node) > 0) {
    loc_idx <- xml_double(loc_node)
    if (measurement_dir == "top down") {
      loc_idx <- nrow(layerFrame) + 1 - loc_idx  # ensure that layerFrame[loc_idx,] is correct with every measurement_dir!
    }

  }

  ## ---Temperature layers----
  layers_temp <- xml_children(xml_find_all(obj, prefix_slash("tempProfile")))
  if (length(layers_temp) > 0) {
    tProfile <- data.table::rbindlist(lapply(layers_temp, extractTempProfile, prefix_plain = prefix_plain))  # yields data.table
    ## again, correct for bottom up measurement direction
    if (measurement_dir == "bottom up")  {  # e.g., snowpilot
      colnames(tProfile)[colnames(tProfile) == "depth"] <- "height"
      tProfile$depth <- rev(tProfile$height)
    }
    ## assure that last row of data.table is at the snow surface:
    tProfile <- tProfile[order(-tProfile$depth)]
    ## if temperature profile is given on same gridf as the snow layers --> merge data.tables
    ## else return individual temperature data.table
    if (isTRUE(all.equal(tProfile$depth, layerFrame$depth))) {
      layerFrame$temperature <- tProfile$temperature
    } else {
      ## calculate height if heightTotal is known:
      if (exists("heightTotal")) {
        tProfile$height <- heightTotal - tProfile$depth  # i.e. height bottom-up at top layer interface
        tProfile <- tProfile[, c(1, 3, 2)]  # sort columns of data.frame
      }
      temperatureProfile <- tProfile
    }
  }

  ## ---Stability tests----
  if (readStabilityTests) {
    testResults <- xml_children(xml_find_all(obj, prefix_slash("stbTests")))
    if (length(testResults) > 0){
      stbFrame <- data.table::rbindlist(lapply(testResults, extractStabilityTests, prefix_plain = prefix_plain), fill = TRUE)  # yields data.table
      stbFrame <- stbFrame[order(stbFrame$depth), ]
      if (exists("heightTotal")) {
        stbFrame$height <- heightTotal - stbFrame$depth  # i.e. height bottom-up at top layer interface
      }
    }
  }

  ## Whumpf Study info:
  whumpf_node <- character()
  whumpf_node <- try({suppressWarnings(xml_find_all(xml_find_first(obj, prefix_plain("metaData")), ".//snowpilot:whumpfData"))}, silent = TRUE)
  if (length(whumpf_node) > 0 & !inherits(whumpf_node, "try-error")) {
    whumpfCracking <- ifelse(xml_text(xml_child(whumpf_node, "whumpfCracking")) == "true", TRUE, FALSE)
    whumpfNoCracking <- ifelse(xml_text(xml_child(whumpf_node, "whumpfNoCracking")) == "true", TRUE, FALSE)
    whumpfNearPit <- ifelse(xml_text(xml_child(whumpf_node, "whumpfNearPit")) == "true", TRUE, FALSE)
    if ((any(c(whumpfCracking, whumpfNoCracking))) & (whumpfNearPit)) {
      whumpfPit <- TRUE
    } else {
      whumpfPit <- FALSE
    }
  }


  ## ---Create snowprofile object----
  SP <- snowprofile(station = station,
                    station_id = station_id,
                    datetime = datetime,
                    elev = elevation,
                    angle = angle,
                    aspect = aspect,
                    latlon = c(lat, lon),
                    hs = heightTotal,
                    layers = snowprofileLayers(hs = heightTotal,
                                               maxObservedDepth = maxObservedDepth,
                                               layerFrame = layerFrame,
                                               validate = FALSE),
                    validate = FALSE)
  if (exists("temperatureProfile")) SP$temperatureProfile <- temperatureProfile
  if (exists("stbFrame")) SP$tests <- snowprofileTests(stbFrame)
  if (!nchar(observer) == 0) SP$observer <- observer
  if (exists("comment_general")) SP$comment_general <- comment_general
  if (exists("comment_location")) SP$comment_location <- comment_location
  if (exists("comment_spack")) {
    SP$comment_snowpack <- comment_spack
    if (exists("HST")) SP$HST <- HST
  }
  if (exists("loc_idx")) SP$layerOfConcern <- loc_idx
  if (exists("whumpfPit")) SP$instabilitySigns <- snowprofileInstabilitySigns(data.frame(type = "whumpf", present = whumpfPit))
  ## some sort of simple rule for 'type' if keywords encountered in station(_id):
  if (is.na(sourceType)) {
    keywords <- c("HRDPS", "vstation", "whiteboard")
    keywords_appear <- sapply(keywords, function(x, keywords) grep(x, c(station, station_id)))

    if (sum(c(keywords_appear$HRDPS, keywords_appear$vstation) > 0)) {
      SP$type = "vstation"
    } else if (sum(keywords_appear$whiteboard) > 0) {
      SP$type = "whiteboard"
    }
  } else {
      if (!sourceType %in% c("manual", "vstation", "aggregate", "whiteboard")) stop("Unknown sourceType!")
      SP$type <- sourceType
  }


  ## final validation of snowprofile:
  if (validate) {
    SP <- reformat_snowprofile(SP)
    validate_snowprofile(SP)
  }


  return(SP)
}
