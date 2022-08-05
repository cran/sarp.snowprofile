#' Read routine for advanced csv tables containing various snowprofile information
#'
#' This routine reads blocks of snowprofile metadata, layers, tests, and stability signs. Columns contain different variables,
#' rows different observations. While metadata only contains one row, layers, tests, and signs consist of potentially multiple
#' rows. Within each block of information, mind the correct alignment of rows. Missing values (i.e., NA) need to be left blank
#' or called `NA`. See the examples below including the example file shipped with the package.
#'
#' @author fherla
#' @param csvFile 'path/to/file.csv'
#' @param meta column names of block metadata
#' @param layers column names of block [snowprofileLayers]
#' @param tests column names of block [snowprofileTests]
#' @param instabilitySigns column names of block [snowprofileInstabilitySigns]
#' @param sep csv column separator
#' @param elev.units if set to "ft", the routine will convert to "m". Set to "m" (or anything else) if it should be unchanged
#' @param tz time zone (default = 'UTC')
#'
#' @examples
#' ## load example csv file that ships with package:
#' csvFile <- system.file('extdata', 'example_adv.csv', package = 'sarp.snowprofile')
#'
#' profile <- snowprofileCsv_advanced(csvFile, meta = c("uid", "hs", "maxObservedDepth", "comment",
#'                                                      "datetime", "zone", "station",
#'                                                      "station_id", "aspect", "elev", "angle"))
#'
#' plot(profile)
#'
#' @export
# TODO: include proper handling of timezones for creating POSIXct datetime objects
snowprofileCsv_advanced <- function(csvFile,
                                    meta = c("uid", "hs", "maxObservedDepth", "comment"),
                                    layers = c("depth", "height", "gtype", "hardness", "datetag", "gsize", "gtype_sec", "layer_comment"),
                                    tests = c("test", "result", "fract_char", "score", "test_depth", "test_comment"),
                                    instabilitySigns = c("instabilitySign_type", "instabilitySign_present", "instabilitySign_comment"),
                                    sep = ",",
                                    elev.units = "ft",
                                    tz = "UTC") {
  ## ---Initialization----
  content <- read.csv(file = csvFile, header = TRUE, sep = sep)

  ## ---Formatting----
  ## check for height or depth
  height_known <- ifelse("height" %in% colnames(content), TRUE, FALSE)
  if (height_known) {
    layers <- layers[layers != "depth"]
    vloc <- "height"
    mdir <- "bottomUp"
  } else {
    layers <- layers[layers != "height"]
    vloc <- "depth"
    mdir <- "topDown"
  }

  NAlayerRows <- which(rowSums(is.na(content[, layers]) | content[, layers] == "") == length(layers))
  nLayers <- ifelse(length(NAlayerRows) == 0, length(content[, vloc]), min(NAlayerRows) - 1)
  if (nLayers < 1) warning("There's not a single layer in your profile!")

  ## ensure correct layer order (i.e. last layer @snow surface):
  k <- if ((mdir == "bottomUp" & (content[1, vloc] > content[nLayers, vloc])) |
           (mdir == "topDown" & (content[1, vloc] < content[nLayers, vloc]))) {
    seq(nLayers, 1, -1)  # i.e. reverse order
  } else {
    seq(nLayers)  # i.e. order is correct as in csv file
  }

  ## Deal with missing columns
  tmp <- meta[which(!meta %in% colnames(content))]
  tmp <- c(tmp, layers[which(!layers %in% colnames(content))])
  tmp <- c(tmp, tests[which(!tests %in% colnames(content))])
  tmp <- c(tmp, instabilitySigns[which(!instabilitySigns %in% colnames(content))])
  if (length(tmp) > 0) warning(paste("Desired column(s) missing in csv file:", paste(tmp, collapse = ", ")))
  meta <- meta[which(meta %in% colnames(content))]
  layers <- layers[which(layers %in% colnames(content))]
  tests <- tests[which(tests %in% colnames(content))]
  instabilitySigns <- instabilitySigns[which(instabilitySigns %in% colnames(content))]

  ## Deal with missing (empty) strings
  content[content == ""] <- NA

  ## --- Layers----
  layerFrame <- content[k, layers]
  cnames <- colnames(layerFrame)

  if ("gtype" %in% layers) layerFrame[, "gtype"] <- as.factor(layerFrame[, "gtype"])
  if ("gtype_sec" %in% layers) layerFrame[, "gtype_sec"] <- as.factor(layerFrame[, "gtype_sec"])
  if ("hardness" %in% layers) layerFrame[, "hardness"] <- sapply(layerFrame[, "hardness"], char2numHHI)
  if ("datetag" %in% layers) layerFrame[, "datetag"] <- as.POSIXct(layerFrame[, "datetag"], tz = tz)
  if ("gsize" %in% layers) layerFrame[, "gsize"] <- as.double(layerFrame[, "gsize"])
  if ("layer_comment" %in% layers) cnames[cnames == "layer_comment"] <- "comment"

  hs <- ifelse("hs" %in% meta, content[1, "hs"], as.double(NA))
  maxObservedDepth <- ifelse("maxObservedDepth" %in% meta, content[1, "maxObservedDepth"], as.double(NA))

  colnames(layerFrame) <- cnames
  SPlayers <- snowprofileLayers(layerFrame = layerFrame, hs = hs, maxObservedDepth = maxObservedDepth)

  ## --- Tests ----
  testsFrame <- content[, tests]
  cnames <- colnames(testsFrame)
  if ("test" %in% tests) {
    cnames[cnames == "test"] <- "type"
  }
  if ("test_depth" %in% tests) {
    cnames[cnames == "test_depth"] <- "depth"
  }
  if ("test_comment" %in% tests) {
    cnames[cnames == "test_comment"] <- "comment"
  }

  colnames(testsFrame) <- cnames
  SPtests <- snowprofileTests(testsFrame)

  ## --- Instability Signs ----
  signsFrame <- content[, instabilitySigns]
  cnames <- colnames(signsFrame)
  if ("instabilitySign_type" %in% instabilitySigns) {
    cnames[cnames == "instabilitySign_type"] <- "type"
  }
  if ("instabilitySign_present" %in% instabilitySigns) {
    cnames[cnames == "instabilitySign_present"] <- "present"
  }
  if ("instabilitySign_comment" %in% instabilitySigns) {
    cnames[cnames == "instabilitySign_comment"] <- "comment"
  }

  colnames(signsFrame) <- cnames
  SPsigns <- snowprofileInstabilitySigns(signsFrame)

  ## --- Metadata----
  meta_auto <- meta[meta %in% c("station", "station_id", "datetime", "tz", "latlon", "elev", "angle", "aspect", "type", "band", "zone", "hs", "maxObservedDepth")]
  meta_manual <- meta[! meta %in% meta_auto]
  SPmeta <- as.list(content[1, meta_auto])
  names(SPmeta) <- meta_auto
  if ("aspect" %in% meta_auto) SPmeta[["aspect"]] <- char2numAspect(SPmeta[["aspect"]])
  if ("datetime" %in% meta_auto) SPmeta[["datetime"]] <- as.POSIXct(SPmeta[["datetime"]], tz = tz)
  if ("elev" %in% meta_auto) {
    if (elev.units == "ft") SPmeta[["elev"]] <- round(SPmeta[["elev"]] / 3.281)
  }
  if ("angle" %in% meta_auto) SPmeta[["angle"]] <- as.double(SPmeta[["angle"]])
  if ("hs" %in% meta_auto) SPmeta[["hs"]] <- as.double(SPmeta[["hs"]])
  if ("maxObservedDepth" %in% meta_auto) SPmeta[["maxObservedDepth"]] <- as.double(SPmeta[["maxObservedDepth"]])

  SP <- do.call("snowprofile", c(list(layers = SPlayers, tests = SPtests, instabilitySigns = SPsigns),
                                 SPmeta))

  for (i in seq(length(meta_manual))) {
    SP[[meta_manual[i]]] <- content[1, meta_manual[i]]
  }



  return(SP)
}
