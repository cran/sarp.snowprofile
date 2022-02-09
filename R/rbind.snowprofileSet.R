#' Concatenate snowprofileSet into a large data.frame with a row for each layer
#'
#' A wrapper to apply [rbind.snowprofile] to each profile in a [snowprofileSet] then concatenate
#'
#' @param ... Object of class [snowprofileSet]
#' @param deparse.level Argument for generic rbind method
#'
#' @details Returns a large data.frame with a row for each layer and additional columns with metadata (calculated with [summary.snowprofile])
#'
#' @seealso [summary.snowprofile], [rbind.snowprofile]
#'
#' @return data.frame
#'
#' @author shorton
#'
#' @examples
#'
#' ## Create rbind table
#' ProfileTable <- rbind(SPgroup)
#' head(ProfileTable)
#'
#' ## Filter by layer properties
#' SHlayers <- subset(ProfileTable, gtype == 'SH')
#' summary(SHlayers)
#' plot(elev ~ gsize, SHlayers)
#'
#' @export
#'
#' @import data.table
#'
rbind.snowprofileSet <- function(..., deparse.level = 1) {

  Profiles <- list(...)[[1]]

  ## If it's a single profile place it in a list so below code works
  if (!is(Profiles, "snowprofileSet"))  stop('rbind.snowprofileSet requires an object of class snowprofileSet')

  ## Get rbind version of each individual profile
  ProfileTable <- lapply(Profiles, rbind)

  ## Rbind into large data.frame
  ProfileTable <- data.table::rbindlist(ProfileTable, fill = TRUE)
  ProfileTable <- as.data.frame(ProfileTable)

  ## Return big data.frame
  return(ProfileTable)
}
