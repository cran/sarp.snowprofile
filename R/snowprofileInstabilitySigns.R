## ####################################################

## snowprofileInstabilitySigns class definition

#######################################################
## authors: fherla


#' Check class snowprofileInstabilitySigns
#'
#' Check if object is of class [snowprofileInstabilitySigns]
#'
#' @param x object to test
#'
#' @return boolean
#'
#' @export
#'
is.snowprofileInstabilitySigns <- function(x) inherits(x, "snowprofileInstabilitySigns")



#' Constructor for a snowprofileInstabilitySigns object
#'
#' Create a snowprofileInstabilitySigns object. Instability signs can for example be whumpfs, cracking, natural avalanches, skier accidental release, ski cutting, etc.
#' For more information, see
#' Canadian Avalanche Association. (2016). Observation Guidelines and Recording Standards for Weather, Snowpack, and Avalanches. Revelstoke, BC, Canada.
#'
#' Note: This class might be a temporary solution to digitize instability signs observed in proximity to snowprofiles.
#' The information contained here, might be ported to a more general field observations class that is both independent from
#' snowprofile objects and that is more in line with existing field observation standards.
#'
#'
#' @param signsFrame a data.frame listing snowpack stability signs. Rows correspond to individual observations of instability signs
#' and columns describe at least the fields `c("type", "present")`.
#'
#'   * type: Sc, Sa, Na, whumpf, crack, ...
#'   * present: Was the instability sign present (TRUE), not present (FALSE), or unknown (NA), for example
#'     - natural avalanches occurred (i.e., Na TRUE), did not occur (i.e., Na FALSE), no observations were carried out (i.e., Na NA)
#'     - skiing the slope led to an avalanche (i.e., Sa TRUE)
#'     - ski cutting did not release avalanche (i.e., Sc FALSE)
#'     - etc
#'
#' @param dropNAs Should empty, non-mandatory columns be dropped from the final snowprofileInstabilitySigns object?
#'
#' @return snowprofileInstabilitySigns object
#'
#' @author fherla
#'
#' @seealso [snowprofile], [snowprofileLayers], [snowprofileTests]
#'
#' @examples
#' ## create a data.frame with instability sign observations
#' (signsFrame <- data.frame(type = c("Na", "whumpf", "cracking", "Sa"),
#'                          present = c(FALSE, TRUE, FALSE, FALSE)))
#'
#' ## create snowprofileInstabilitySigns object
#' instabilitySigns <- snowprofileInstabilitySigns(signsFrame)
#'
#' ## create snowprofile object containing instability signs and check resulting object:
#' snowprofile(instabilitySigns = instabilitySigns)
#'
#' @export
#'
snowprofileInstabilitySigns <- function(signsFrame = data.frame(type = as.character(NA),
                                                                present = as.character(NA),
                                                                comment = as.character(NA)),
                             dropNAs = TRUE) {

  ## assert data.frame (code breaks with data.table)
  if (!is.data.frame(signsFrame)) stop("signsFrame needs to be a data.frame")
  signsFrame <- as.data.frame(signsFrame)
  ## assert column names:
  mandatory_colnames <- c("type", "present")
  mcol_idx_missing <- which(!mandatory_colnames %in% colnames(signsFrame))
  if (length(mcol_idx_missing) > 0) {
    stop(paste("Mandatory field is missing:", paste(mandatory_colnames[mcol_idx_missing], collapse = ", ")))
  }

  ## type conversion:
  dtypes <- c(type = "character", present = "logical", comment = "character")
  cols <- colnames(signsFrame)
  for (col in cols) {  # convert standard object columns to correct classes
    if (col %in% names(dtypes)){
      signsFrame[, col] <- do.call(paste0("as.", unname(dtypes[col])), list(x = signsFrame[, col]))
    }
  }

  ## type conversion:
  sapply(c("type"), function(x) signsFrame[, x] <- as.character(signsFrame[, x]))
  sapply(c("present"), function(x) signsFrame[, x] <- as.logical(signsFrame[, x]))

  ## make sure whumpfing and cracking get always spelled the same way:
  whumpf_id <- which(grepl("whumpf", signsFrame$type, ignore.case = TRUE))
  signsFrame$type[whumpf_id] <- "whumpf"

  crack_id <- which(grepl("crack", signsFrame$type, ignore.case = TRUE))
  signsFrame$type[crack_id] <- "crack"

  ## drop NAs and run checks:
  if (dropNAs) {
    ## keep mandatory columns and columns that contain at least one row of observation
    signsFrame <- signsFrame[, unique(c(which(colnames(signsFrame) %in% mandatory_colnames), which(colSums(is.na(signsFrame)) < nrow(signsFrame))))]
  }
  ## drop NA rows
  signsFrame <- signsFrame[rowSums(is.na(signsFrame)) < ncol(signsFrame), ]

  class(signsFrame) <- append("snowprofileInstabilitySigns", class(signsFrame))
  return(signsFrame)
}
