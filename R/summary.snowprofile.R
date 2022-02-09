#' Summary of a single snowprofile
#'
#' @param object snowprofile object
#' @param fast boolean switch for twice as fast computation. downside: keep only length-1 meta data, i.e., discard latlon, or nlayers..
#' @param ... additional arguments for generic method
#'
#' @return data.frame
#'
#' @details
#'
#' Creates a one row data.frame where each column contains metadata.
#'
#' Metadata is determines as elements of the snowprofile object list that are length = 1. An exception is made for latlon where separate columns for lat and lon are produces.
#'
#' A derived value `nLayers` is derived by counting the number of rows in $layers.
#'
#' @seealso [summary.snowprofileSet]
#'
#' @author shorton
#'
#' @examples
#'
#' Profile <- SPgroup[[1]]
#' names(Profile)
#' summary(Profile)
#' lapply(SPgroup, summary)
#'
#' @export
#'
summary.snowprofile <- function(object, fast = FALSE, ...) {

  if (!fast) {
    # Initialize data.frame with a 1 row long dummy variable
    Metadata <- data.frame(init = NA)

    # Loop through each element in profile
    for (col in names(object)) {

      Element <- object[col]

      # Copy element if it's length is one
      if (lengths(Element) == 1) {
        Metadata[col] <- Element

        # Special treatment for latlon
      } else if (col == "latlon") {
        Metadata$lat <- object$latlon[1]
        Metadata$lon <- object$latlon[2]

        # Place any summary stats of the layers here
      } else if (col == "layers") {
        Metadata$nLayers <- nrow(object$layers)
      }
    }

    # Delete dummy variable
    Metadata$init <- NULL
  } else {
    ## fast computation method:
    lengthProperties <- sapply(object, length)

    Metadata <- data.frame(object[which(lengthProperties == 1)])
  }

  # Return metadata data.frame
  return(Metadata)

}
