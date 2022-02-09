#' Convert snowprofile into data.frame with columns for metadata
#'
#' Convert snowprofile object into data.frame with a row for each layer and additional columns with metadata
#'
#' @param ... Object of class [snowprofile]
#' @param deparse.level Argument for generic rbind method
#'
#' @details Metadata columns are calculated with [summary.snowprofile]
#'
#' @seealso [summary.snowprofile], [rbind.snowprofileSet]
#'
#' @return data.frame
#'
#' @author shorton
#'
#' @examples
#'
#' Profile <- SPgroup[[1]]
#' ProfileTable <- rbind(Profile)
#' head(ProfileTable)
#'
#' @export
#'
rbind.snowprofile <- function(..., deparse.level = 1) {

  Profile <- list(...)[[1]]

  ## If it's a single profile place it in a list so below code works
  if (!is(Profile, "snowprofile")) stop('rbind.snowprofile requires an object of class snowprofile')

  ## Merge metadata with layer data
  ProfileTable <- merge(summary(Profile), Profile$layers, all.y = TRUE)

  ## Return data.frame
  return(ProfileTable)
}
