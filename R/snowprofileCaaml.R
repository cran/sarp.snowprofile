#' Read a Caaml file into a snowprofile object
#'
#' @import data.table xml2
#'
#' @param caamlFile 'path/to/file.caaml'
#' @param sourceType choose 'manual', 'modeled', 'vstation', 'aggregate' or 'whiteboard'; if NA, the default will be chosen by [snowprofile].
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
#'
#' @export

## TODO: incorporate stability test data, if available!

snowprofileCaaml <- function(caamlFile,
                             sourceType = NA) {

  ## --- Initialization, assertions, subfunctions ----

  extractLayerCharacteristics <- function(layer_element) {
    ## extracts most important layer characteristics of one given SnowProfile layer @param layer_element xml_nodeset of SnowProfile layer
    ## @return data frame containing either double prec float or string, as appropriate

    ## 'lwc' or 'wetness':
    wTry <- xml_text(xml_child(layer_element, "caaml:wetness"))
    lwcTry <- xml_text(xml_child(layer_element, "caaml:lwc"))
    lwc_write <- ifelse(all(is.na(lwcTry)), yes = wTry, no = lwcTry)

    ## split layer comments into fields:
    lay_comment <- xml_text(xml_child(xml_find_first(layer_element, "caaml:metaData"), "caaml:comment"))
    fields <- strsplit(lay_comment, " ")[[1]]
    bdate <- fields[grep("....-..-..", fields)]
    if (length(bdate) == 0) bdate <- NA
    else bdate <- as.POSIXct(bdate)
    shovelTest <- fields[grep("ST", fields)]
    if (length(shovelTest) == 0) ST <- NA
    else ST <- as.numeric(strsplit(shovelTest, "ST")[[1]][2])

    ## return data.frame (those columns that actually contain values)
    layerFrame <- data.frame(depth = xml_double(xml_child(layer_element, "caaml:depthTop")),
                             thickness = xml_double(xml_child(layer_element, "caaml:thickness")),
                             hardness = char2numHHI(xml_text(xml_child(layer_element, "caaml:hardness"))),
                             gtype = as.character(xml_text(xml_child(layer_element, "caaml:grainFormPrimary"))),
                             gtype_sec = xml_text(xml_child(layer_element, "caaml:grainFormSecondary")),
                             gsize = xml_double(xml_child(xml_child(xml_child(layer_element, "caaml:grainSize"), "caaml:Components"), "caaml:avg")),
                             gsize_max = xml_double(xml_child(xml_child(xml_child(layer_element, "caaml:grainSize"), "caaml:Components"), "caaml:avg")),
                             bdate = bdate,
                             ST = ST,
                             lwc = lwc_write,
                             stringsAsFactors = TRUE)
    layerFrame <- Filter(function(x) !all(is.na(x)), layerFrame)
    return(layerFrame)
  }

  extractTempProfile <- function(layer_element) {
    ## extracts depth and temperature of one given SnowProfile 'tempProfile' layer and returns data.frame, analogously to above
    tFrame <- data.frame(depth = xml_double(xml_child(layer_element, "caaml:depth")),
                         temperature = xml_double(xml_child(layer_element, "caaml:snowTemp")))
    tFrame <- Filter(function(x) !all(is.na(x)), tFrame)
    return(tFrame)
  }

  ## read caaml file and assert that it's '.caaml' and 'SnowProfile':
  obj <- read_xml(caamlFile)

  splittedFN <- strsplit(tolower(caamlFile), split = "\\.")[[1]]
  try(if (!splittedFN[length(splittedFN)] == "caaml") stop())
  try(if (xml_name(obj) != "SnowProfile") stop(paste0("Content of '", caamlFile,
                                                      "' doesn't seem to be a 'SnowProfile'")))

  ## Extract and write meta-data:
  ## ---Time ----
  datetimeRAW <- gsub("T", " ", as.character(xml_text(xml_find_all(obj, ".//caaml:timePosition"))))
  datetime <- as.POSIXct(substr(datetimeRAW, 1, 23), tz = "UTC")


  ## ---Source and comments----
  observer <- paste(xml_text(xml_contents(xml_contents(xml_find_all(obj, ".//caaml:srcRef")))), collapse = "|")
  try({com <- xml_text(xml_child(xml_find_first(obj, "caaml:metaData")), "caaml:comment")
       if (!is.na(com)) comment_general <- com}, silent = TRUE)
  try({com_loc <- xml_text(xml_child(xml_child(xml_find_all(obj, "caaml:locRef"), "caaml:metaData"), "caaml:comment"))
       if (!is.na(com_loc)) comment_location <- com_loc}, silent = TRUE)


  ## ---Location----
  ## maybe still naming problems with some software?
  ## include here: parent locRef and child ObsPoint
  locRefName <- xml_child(xml_find_all(obj, ".//caaml:locRef"), "caaml:name")
  if (is.na(locRefName)) {
    station <- xml_text(xml_child(xml_child(xml_find_all(obj, ".//caaml:locRef"), "caaml:ObsPoint")), "caaml:name")
    station_id <- xml_attr(xml_child(xml_find_all(obj, ".//caaml:locRef"), "caaml:ObsPoint"), "id")
  } else {
    station <- xml_text(locRefName)
    station_id <- xml_attr(xml_find_all(obj, ".//caaml:locRef"), "id")
  }

  ## ---Elevation, aspect, angle, latlon----
  elevation <- xml_double(xml_child(xml_find_all(obj, ".//caaml:ElevationPosition"), "caaml:position"))
  if (length(elevation) == 0) elevation <- as.numeric(NA)
  angle <- xml_double(xml_child(xml_find_all(obj, ".//caaml:SlopeAnglePosition"), "caaml:position"))
  if (length(angle) == 0) angle <- as.numeric(NA)
  aspect <- xml_double(xml_child(xml_find_all(obj, ".//caaml:AspectPosition"), "caaml:position"))
  if (length(aspect) == 0) aspect <- as.numeric(NA)
  lonlat <- strsplit(xml_text(xml_find_all(obj, ".//gml:pos")), split = " ")
  if (length(lonlat) == 0) {
    lat <- as.numeric(NA)
    lon <- as.numeric(NA)
  } else {
    lat <- as.double(lonlat[[1]][2])
    lon <- as.double(lonlat[[1]][1])
  }

  ## ---profile Depth or SnowHeight, HST----
  hS <- xml_find_all(obj, ".//caaml:hS")
  ## sometimes, hS can contain a 'components' nodeset with the actual hS stored as a child caaml:height:
  heightTotal <- tryCatch(xml_double(hS),
                          warning = function(w) {
                            choices <- c(xml_double(xml_child(hS, ".//caaml:height")),
                                         xml_double(xml_child(hS, ".//caaml:snowHeight")))
                            choices[!is.na(choices)]})

  pD <- xml_double(xml_find_all(obj, ".//caaml:profileDepth"))
  if (length(pD) == 1) {
    if (!isTRUE(all.equal(heightTotal, pD))) profileDepth <- pD
  }

  ## snowpack conditions (i.a. HST):
  try({spackCondMeta_node <- xml_child(xml_find_first(obj, ".//caaml:snowPackCond"), "caaml:metaData")
      if (!is.na(spackCondMeta_node)) {
        comment_spack <- xml_text(xml_child(spackCondMeta_node), "caaml:comment")
        com_HST_check <- grep("HST[0-9]{1,3}", comment_spack)  # returns index if pattern 'HST###' exists
        if (length(com_HST_check) != 0) {
          com_HST <- gsub(".*(HST[0-9]{1,3}).*", "\\1", comment_spack)  # returns 'HST###'
          HST <- as.numeric(strsplit(com_HST, "HST")[[1]][2])
        }
      }
    }, silent = TRUE)

  ## ---Snow layers----
  ## Extract layers, arange in data.frame (subfunction) and write to snowprofile object:
  layers_snow <- xml_children(xml_find_all(obj, ".//caaml:stratProfile"))
  layerFrame <- data.table::rbindlist(lapply(layers_snow, extractLayerCharacteristics), fill = TRUE)  # yields data.table
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


  ## ---Temperature layers----
  layers_temp <- xml_children(xml_find_all(obj, ".//caaml:tempProfile"))
  if (length(layers_temp) > 0) {
    tProfile <- data.table::rbindlist(lapply(layers_temp, extractTempProfile))  # yields data.table
    ## assure that last row of data.table is at the snow surface:
    tProfile <- tProfile[order(-tProfile$depth)]
    ## if temperature profile is given on same grid as the snow layers --> merge data.tables
    ## else return individual temperature data.table
    if (isTRUE(all.equal(tProfile$depth, layerFrame$depth))) {
      layerFrame$temperature <- tProfile$temperature
    } else {
      ## calculate height if heightTotal is known:
      if (exists("heightTotal")) tProfile$height <- heightTotal - tProfile$depth # i.e. height bottom-up at top layer interface
      temperatureProfile <- tProfile
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
                                               layerFrame = layerFrame))
  if (exists("temperatureProfile")) SP$temperatureProfile <- temperatureProfile
  if (!nchar(observer) == 0) SP$observer <- observer
  if (exists("profileDepth")) SP$profileDepth <- profileDepth
  if (exists("comment_general")) SP$comment_general <- comment_general
  if (exists("comment_location")) SP$comment_location <- comment_location
  if (exists("comment_spack")) {
    SP$comment_snowpack <- comment_spack
    if (exists("HST")) SP$HST <- HST
  }
  ## some sort of simple rule for 'type' if keywords encountered in station(_id):
  if (is.na(sourceType)) {
    keywords <- c("HRDPS", "vstation", "whiteboard")
    keywords_appear <- sapply(keywords, function(x, keywords) grep(x, c(station, station_id)))

    if (sum(c(keywords_appear$HRDPS, keywords_appear$vstation) > 0)) {
      SP$type = "vstation"
    } else if (sum(keywords_appear$whiteboard) > 0) {
      SP$type = "whiteboard"
    } else {
      if (!sourceType %in% c("manual", "vstation", "aggregate", "whiteboard")) stop("Unknown sourceType!")
      SP$type <- sourceType
    }
  }

  return(SP)
}
