## ####################################################

## snowprofileTests class definition

#######################################################
## authors: fherla


#' Check class snowprofileTests
#'
#' Check if object is of class [snowprofileTests]
#'
#' @param x object to test
#'
#' @return boolean
#'
#' @export
#'
is.snowprofileTests <- function(x) inherits(x, "snowprofileTests")



#' Constructor for a snowprofileTests object
#'
#' Create a snowprofileTests object.
#'
#' For more information, see
#' Canadian Avalanche Association. (2016). Observation Guidelines and Recording Standards for Weather, Snowpack, and Avalanches (OGRS). Revelstoke, BC, Canada.
#'
#'
#' @param testsFrame a data.frame listing snowpack stability tests. Rows correspond to individual tests and columns describe at least the
#' fields `c("type", "result", "fract_char", "score", "depth")`.
#'
#'   * Test **type** and **result** yield the standard 'data code' for reporting snowpack tests according to the OGRS (see Details).
#'   Following type and result combinations are allowed:
#'     * STV, STE, STM, STH, STN, and mixed forms STE-M, STM-H
#'     * CTV, CTE, CTM, CTH, CTN, and mixed forms CTE-M, CTM-H
#'     * DTV, DTE, DTM, DTH, DTN, and mixed forms DTE-M, DTM-H
#'     * ECTPV, ECTP, ECTN, ECTX
#'     * RB, PST, DT tests are currently not supported.
#'   * **score**: numeric, number of taps (for CT, ECT)
#'   * **fract_char** corresponds to the fracture character, e.g., SP, SC, PC, RP, BRK, ...
#'   * **depth**: vertical location of corresponding snowpack layer (from surface)
#'   * potential test comment column
#'
#' @param dropNAs Should empty, non-mandatory columns be dropped from the final snowprofileTests object?
#'
#' @return snowprofileTests object
#'
#' @author fherla
#'
#' @seealso [snowprofile], [snowprofileLayers], [snowprofileInstabilitySigns]
#'
#' @examples
#' ## create a data.frame with test observations
#' (testsFrame <- data.frame(type = c("CT", "ST", "ECT"),
#'                          result = c("E-M", "M", "P"),
#'                          score = c(10, NA, 12),
#'                          fract_char = c("SP", NA, NA),
#'                          depth = c(40, 40, 40),
#'                          comment = c("some comment on first test", "", "")))
#'
#' ## create snowprofileTests object
#' tests <- snowprofileTests(testsFrame)
#'
#' ## create snowprofile object containing test results and check resulting object:
#' snowprofile(tests = tests)
#'
#' @export
#'
snowprofileTests <- function(testsFrame = data.frame(type = as.character(NA),
                                                     result = as.character(NA),
                                                     score = as.double(NA),
                                                     fract_char = as.character(NA),
                                                     depth = as.double(NA),
                                                     comment = as.character(NA)),
                             dropNAs = TRUE) {

  ## assert data.frame (code breaks with data.table)
  if (!is.data.frame(testsFrame)) stop("testsFrame needs to be a data.frame")
  testsFrame <- as.data.frame(testsFrame)
  ## assert column names:
  mandatory_colnames <- c("type", "result", "score", "fract_char", "depth")
  mcol_idx_missing <- which(!mandatory_colnames %in% colnames(testsFrame))
  if (length(mcol_idx_missing) > 0) {
    stop(paste("Mandatory field is missing:", paste(mandatory_colnames[mcol_idx_missing], collapse = ", ")))
  }

  ## type conversion:
  # emptySPT <- snowprofileTests(dropNAs = FALSE)  # would be elegant, but ends in an endless loop
  # dtypes <- sapply(emptySPT, function(x) class(x)[1])
  dtypes <- c(type = "character", result = "character", score = "numeric", fract_char = "character",
              depth = "numeric", comment = "character")
  cols <- colnames(testsFrame)
  for (col in cols) {  # convert standard object columns to correct classes
    if (col %in% names(dtypes)){
      testsFrame[, col] <- do.call(paste0("as.", unname(dtypes[col])), list(x = testsFrame[, col]))
    }
  }

  ## drop NAs and run checks:
  if (dropNAs) {
    ## keep mandatory columns and columns that contain at least one row of observation
    testsFrame <- testsFrame[, unique(c(which(colnames(testsFrame) %in% mandatory_colnames), which(colSums(is.na(testsFrame)) < nrow(testsFrame))))]
  }
  ## drop NA rows
  testsFrame <- testsFrame[rowSums(is.na(testsFrame)) < ncol(testsFrame), ]

  ## auto-fill result field for CTs, STs, & DTs if result is NA, but score is given:
  fill_idx <- which(is.na(testsFrame$result) & !is.na(testsFrame$score) & testsFrame$type %in% c("CT", "ST", "DT"))
  testsFrame[fill_idx, "result"] <- ifelse(testsFrame[fill_idx, "score"] <= 10, "E", as.character(NA))

  fill_idx <- which(is.na(testsFrame$result) & !is.na(testsFrame$score) & testsFrame$type %in% c("CT", "ST", "DT"))
  testsFrame[fill_idx, "result"] <- ifelse(testsFrame[fill_idx, "score"] <= 20, "M", as.character(NA))

  fill_idx <- which(is.na(testsFrame$result) & !is.na(testsFrame$score) & testsFrame$type %in% c("CT", "ST", "DT"))
  testsFrame[fill_idx, "result"] <- ifelse(testsFrame[fill_idx, "score"] <= 30, "H", as.character(NA))

  ## check for meaningful combination of type--result (i.e., data code)
  datacodes_supported <- c('STV', 'STE', 'STM', 'STH', 'STN', 'STE-M', 'STM-H',
                           'CTV', 'CTE', 'CTM', 'CTH', 'CTN', 'CTE-M', 'CTM-H',
                           'DTV', 'DTE', 'DTM', 'DTH', 'DTN', 'DTE-M', 'DTM-H',
                           'ECTPV', 'ECTP', 'ECTN', 'ECTX')
  datacodes_provided <- paste0(testsFrame$type, testsFrame$result)
  id_unsupported <- which(! datacodes_provided %in% datacodes_supported)
  if (length(id_unsupported) > 0) warning(paste("Unsupported type--result combination(s):", paste(datacodes_provided[id_unsupported], collapse = ", ")))

  class(testsFrame) <- append("snowprofileTests", class(testsFrame))
  return(testsFrame)
}
