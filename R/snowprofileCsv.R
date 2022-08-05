#' Read csv file into a snowprofile object
#'
#'
#' @param path 'path/to/file.csv'
#' @param header is there a header line in the csv file to explain the column names? If not, specify a character vector of column names in the correct order.
#' @param sep csv column separator as string
#' @param use.swisscode boolean; are grain types given as (numeric) `swisscode` (`TRUE`) or as character strings (`FALSE`)? If `TRUE`, grain types can be given
#' as three-digit code (gt1|gt2|gt3), or as one-digit code specifying the primary grain type *if* another column is provided that specifies crusts.
#' See Details and Examples for more information.
#' @param height character string referring to the csv column of the top layer interfaces
#' @param gtype character string referring to the csv column of the grain types
#' @param hardness character string referring to the csv column of the layer hardnesses
#' @param ... provide name-value pairs of additional csv columns (in the form `gsize = 'csv-GrainSize-ColName'`), e.g.
#'
#'   - profile specific info: `station`, `station_id`, `datetime`, `latlon`, `elev`, `angle`, `aspect`, `type` (see [snowprofile])
#'   - layer specific info: deposition date, grain size, ssi, ... (see [snowprofileLayers])
#'
#' @param crust.val If a column 'crust' is provided, what value of 'crust' defines MFcr? Mostly, either 2 (default) or 1. See Details.
#' @param tz time zone (default = 'UTC')
#' @return snowprofile object
#'
#' @details The minimum information required to construct a valid [snowprofile] object is `height`, `gtype` and `hardness`. Currently, substituting `height` with
#' a depth vector is not supported.
#'
#' If profile specific information is provided in the csv table, it can only be included into the snowprofile object through the exact field names (see above).
#' However, layer specific information can be named arbitrarily (except for the three required fields).
#'
#' Regarding **swisscode**: The SNOWPACK documentation specifies that MFcr are encoded as (gt1|gt2|gt3) = (7|x|2), i.e. gt1 == 7 and gt3 == 2. This is
#' also how this routine handles the grain type encoding per default. However, some csv tables might be provided using swisscode encoding and
#' providing gt1, gt2, and gt3 as individual one-digit columns. In those cases, gt3 could be defined as a boolean (0 or 1), where gt1 == 7 and gt3 == 1
#' represent crusts, instead of the aforementioned standard definition of gt1 == 7 and gt3 == 2. To handle these cases, `crust.val` can be set to 1, instead
#' of its default `crust.val = 2`.
#'
#' @seealso [snowprofileCsv_advanced]
#'
#' @author fherla
#'
#' @examples
#' ## imagine a csv table with a very straightforward format,
#' ## similar to the following data.frame:
#' (DF <- data.frame(height = c(50, 80, 100), gtype = c('FC', 'RG', 'PP'), hardness = c(1, 3, 2)))
#' ## write DF to a temporary file:
#' write.csv(DF, file = file.path(tempdir(), 'file.csv'))
#'
#' ## read this file very easily by
#' profile <- snowprofileCsv(file.path(tempdir(), 'file.csv'))
#' profile
#'
#'
#' ## imagine a csv table that requires a bit more customization,
#' ## similar to the following data.frame:
#' (DF <- data.frame(ID = rep(1234, times = 3), layer_top = c(10.5, 15, 55.0), gt1 = c(5, 7, 2),
#'                   gs = c(5.0, 1.5, 1.0), crust = c(0, 1, 0), hardness = c('F', 'P', '4F+')))
#' write.csv(DF, file = file.path(tempdir(), 'file.csv'))
#'
#' profile <- snowprofileCsv(file.path(tempdir(), 'file.csv'), height = 'layer_top', gtype = 'gt1',
#'                           use.swisscode = TRUE, gsize = 'gs', crust.val = 1)
#' profile
#' ## Note that the csv column 'crust', which specifies whether a MF layer is actually
#' #  a MFcr layer, is already named correctly (i.e., 'crust'). If it were named 'freeze-crust',
#' #  we would need to add to the function call: `crust = 'freeze-crust'`.
#' # Also note, that we need to provide `crust.val = 1`, since we're not using the standard definition
#' # of swisscode MFcr encoding (see Details).
#'
#' ## let's assume you want to read the csv file an customize some names, e.g. GrainSIZE:
#' profile <- snowprofileCsv(file.path(tempdir(), 'file.csv'), height = 'layer_top', gtype = 'gt1',
#'                           use.swisscode = TRUE, GrainSIZE = 'gs')
#' profile
#'
#' ## Note that generally in a snowprofile object layer properties can be custom named,
#' #  meta information, e.g. station_id, can not! I.e. you need to use the prescribed names.
#'
#'
#' @export
snowprofileCsv <- function(path,
                           header = TRUE,
                           sep = ",",
                           use.swisscode = FALSE,
                           height = "height",
                           gtype = "gtype",
                           hardness = "hardness",
                           ...,
                           crust.val = 2,
                           tz = "UTC") {

  ## ---Initialization----
  content <- read.csv(file = path, header = header, sep = sep)
  if (!isTRUE(header)) colnames(content) <- ifelse(!length(header) == ncol(content), stop("Number of colnames does not match number of columns."), header)

  dots <- substitute(...())
  dot_vars <- names(dots)

  ## ---Formatting----
  nLayers <- length(content[, height])
  ## ensure correct layer order (i.e. last layer @snow surface):
  k <- if (content[1, height] > content[nLayers, height]) {
    seq(nLayers, 1, -1)  # i.e. reverse order
  } else {
    seq(nLayers)  # i.e. order is correct as in csv file
  }

  ## grain types are given not as character strings, but as swiss numeric code
  if (use.swisscode) {
    if (content[1, gtype] >= 100) { # three digit format: gt1|gt2|MFcr
      gt1 <- sapply(content[, gtype], function(x) as.numeric(unlist(strsplit(as.character(x), ""))[[1]]))
      gt2 <- sapply(content[, gtype], function(x) as.numeric(unlist(strsplit(as.character(x), ""))[[2]]))
      gt3 <- sapply(content[, gtype], function(x) as.numeric(unlist(strsplit(as.character(x), ""))[[3]]))
      content[, "gtype_sec"] <- as.factor(swisscode[gt2])
      dots["gtype_sec"] <- "gtype_sec"
      dot_vars <- c(dot_vars, "gtype_sec")
      content[, gtype] <- as.factor(ifelse((gt1 == 7) & (gt3 == 2), "MFcr", swisscode[gt1]))
    } else {
      # only gt1-digit provided need another column containing MFcr information
      if (!"crust" %in% dot_vars) {
        if (!"crust" %in% names(content)) stop("use.swisscode is TRUE, but the grain type is not given as three-digit code, nor is there a column 'crust'.")
        crust <- "crust"
      }
      content[, gtype] <- as.factor(ifelse((content[, gtype] == 7) & (content[, crust] == crust.val), "MFcr", swisscode[content[, gtype]]))
    }
  }

  ## accommodate character strings as hardness
  if (is.character(content[, hardness])) {
    content[, hardness] <- sapply(content[, hardness], char2numHHI)
  } else if (is.factor(content[, hardness])) {
    content[, hardness] <- sapply(as.character(content[, hardness]), char2numHHI)
  }

  ## find dot variables for snowprofile and snowprofileLayers call:
  profile_vars <- dot_vars[dot_vars %in% c("station", "station_id", "datetime", "tz", "latlon", "elev", "angle", "aspect", "type", "band", "zone")]
  layer_vars <- dot_vars[!dot_vars %in% profile_vars]

  ## ---Create snowprofile object----
  ## create call to snowprofileLayers:
  layer_dots <- as.pairlist(layer_vars)
  if (length(layer_vars) >= 1) names(layer_dots) <- layer_vars
  for (lv in layer_vars) {
    layer_dots[[lv]] <- content[k, dots[[lv]]]
  }
  SPlayers <- do.call("snowprofileLayers", c(list(height = content[k, height],
                                                  gtype = content[k, gtype],
                                                  hardness = content[k, hardness]),
                                             layer_dots))

  ## final call to snowprofile constructor
  profile_dots <- as.pairlist(profile_vars)
  if (length(profile_vars) >= 1) names(profile_dots) <- profile_vars
  for (pv in profile_vars) {
    if (pv == "datetime") profile_dots[[pv]] <- as.POSIXct(content[1, dots[[pv]]], tz = tz)
    else profile_dots[[pv]] <- content[1, dots[[pv]]]
  }
  SP <- do.call("snowprofile", c(list(layers = SPlayers),
                                 profile_dots))


  return(SP)
}
