#' Summarize multiple snowprofiles
#'
#' Wrapper for [summary.snowprofile], which only returns metadata for a single snowprofile object. summary.snowprofileSet provides metadata for multiple snowprofiles, which is useful for subsetting.
#'
#' @param object list of snowprofile objects
#' @param ... additional arguments for generic method
#'
#' @return data.frame
#'
#' @author shorton
#'
#' @seealso [summary.snowprofile], [rbind.snowprofileSet]
#'
#' @examples
#'
#' ## Extract metadata for a group of profiles
#' Metadata <- summary(SPgroup)
#' head(Metadata)
#'
#' ## Subsetting profiles with Metadata
#' Alpine <- SPgroup[Metadata$elev > 2000]
#' summary(Alpine)
#' Shallow <- SPgroup[Metadata$hs < 150]
#' summary(Shallow)
#' Week2 <- SPtimeline[summary(SPtimeline)$date > '2017-12-15']
#'
#' @export
#'
#' @import data.table
#'
summary.snowprofileSet <- function(object, ...) {

  ## Produce list of summaries of individual profiles
  Summaries <- lapply(object, summary)
  Summaries <- data.table::rbindlist(Summaries, fill = TRUE)
  Summaries <- as.data.frame(Summaries)

  return(Summaries)
}
