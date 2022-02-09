#' Export or write a snowprofile object to a CSV table
#'
#'
#' @param profile [snowprofile] object
#' @param filename character string, e.g. 'path/to/file.csv'
#' @param sep csv column separator as character string
#' @param export.all one of `TRUE`, `FALSE`, `'Layers'`: export all variables of the snowprofile object to the csv table?
#'
#' If `'Layers'`, then all layer variables of the snowprofile will be exported.
#' @param variables A tag-value list of the format, e.g. height = 'height_top', to specify column names of specific variables,
#' to customize column order, and/or to include specific profile meta data if `export.all == 'Layers'`
#' (e.g. easily include the meta data `station_id`). Note that the
#' tags of the tag-value list need to correspond to elements of the snowprofile object.
#'
#' @details Note that existing files with the specified filename will be **overwritten** without warning!
#'
#' @seealso [snowprofileCsv]
#'
#' @return Writes csv file to disk, no return value in R
#'
#' @author fherla
#'
#' @examples
#'
#' ## export an entire snowprofile object:
#'
#' export.snowprofileCsv(SPpairs$A_manual, filename = file.path(tempdir(), 'file.csv'),
#'                       export.all = TRUE)
#'
#'
#' ## export only the layer properties of a snowprofile object,
#' #  and change the column order with few column names:
#' #  All layer variables will be exported, but the three ones provided in 'variables'
#' #  will be the first three columns of the csv table, and their column names will be changed
#' #  accordingly.
#'
#' export.snowprofileCsv(SPpairs$A_manual, filename = file.path(tempdir(), 'file.csv'),
#'                       export.all = 'Layers',
#'                       variables = list(height = 'height_top', hardness = 'hardness',
#'                                        gtype = 'gt1'))
#'
#'
#' ## export all layer properties of a snowprofile object plus the station ID:
#'
#' export.snowprofileCsv(SPpairs$A_manual, filename = file.path(tempdir(), 'file.csv'),
#'                       export.all = 'Layers', variables = list(station_id = 'station_id'))
#'
#' ## check the content of the exported csv file:
#' csv_content <- read.csv(file.path(tempdir(), 'file.csv'))
#' head(csv_content)
#'
#' ## or re-import the csv file as snowprofile object:
#' csv_snowprofile <- snowprofileCsv(file.path(tempdir(), 'file.csv'))
#' print(csv_snowprofile)
#'
#' @export
export.snowprofileCsv <- function(profile,
                                  filename = stop("filename must be provided"),
                                  sep = ",",
                                  export.all = "Layers",
                                  variables = NA) {

  ## ---Assertions----
  stopifnot(is.snowprofile(profile))
  if (!all(is.na(variables)))
    if (!is.list(variables))
      stop("variables must be either NA or a tag-value list of the format: height = 'height_top'; see ?export.snowprofileCsv")
  if (!all(names(variables) %in% c(names(profile), names(profile$layers)))) {
    warning("At least one of your specified variable names does not exist in the snowprofile object, ignoring...")
    variables <- variables[names(variables) %in% c(names(profile), names(profile$layers))]
  }

  ## ---Formatting----
  ## get an array of variables to export
  if (isTRUE(export.all)) {
    vexp <- c(names(profile$layers), names(profile))
  } else if (isFALSE(export.all)) {
    vexp <- names(variables)
  } else if (export.all == "Layers") {
    vexp <- unique(c(names(profile$layers), names(variables)))
  } else {
    stop("export.all must either be TRUE, FALSE or 'Layers'")
  }

  ## create data.frame
  layers <- profile$layers[vexp[which(vexp %in% names(profile$layers))]]
  meta <- profile[vexp[which(vexp %in% names(profile))]]
  if ((length(layers) > 0) && (length(meta) > 0)) {
    DF <- data.frame(layers, meta)
  } else if ((length(layers) > 0) && (length(meta) == 0)) {
    DF <- data.frame(layers)
  } else if ((length(layers) == 0) && (length(meta) > 0)) {
    DF <- data.frame(meta)
  }

  ## rename colnames according to variables
  if (is.list(variables)) {
    cn <- colnames(DF)
    cn[which(cn %in% names(variables))] <- as.character(variables[cn[which(cn %in% names(variables))]])
    colnames(DF) <- cn
    ## sort columns according to variables
    DF <- DF[, as.character(c(variables, cn[!cn %in% variables]))]
  }

  ## ---Write to disk----
  write.csv(DF, file = filename, row.names = FALSE, quote = FALSE)

}
