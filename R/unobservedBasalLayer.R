#' Insert a special layer at the bottom to indicate a snow profile that's unobserved from a specific point down to the ground
#' internal function, not exported. used in snowprofileLayers
#'
#' @param object [snowprofileLayers] object
#' @param basal_offset a positive numeric scalar indicating the thickness of the basal unobserved layer(s)
#' @param setBasalThicknessNA boolean TRUE/FALSE indicating whether the thickness of the inserted layer should be `basal_offset` or `NA`.
#' Setting the thickness to NA corresponds to setting a flag that the depth of the profile (i.e., the unobserved basal layers) is unknown.
#' This often happens in manual profiles which only observe the uppermost meter (or so) of the snowpack
#' @return same object with basal layer inserted as individual row in the data.frame
#' @author fherla
insertUnobservedBasalLayer <- function(object, basal_offset, setBasalThicknessNA = FALSE) {

  if (!is.snowprofileLayers(object)) stop("object needs to be a snowprofileLayers object")
  if (!is.double(basal_offset)) {
    stop("basal_offset needs to be positive numeric")
  } else if (basal_offset < 0) {
    stop("basal_offset needs to be postive")
  }

  object[seq(2, nrow(object)+1), ] <- object[seq(1, nrow(object)), ]
  object[1, ] <- NA

  object[1, "height"] <- basal_offset
  object[1, "thickness"] <- object[1, "height"]
  if (exists("depth", where = object)) {
    object[1, "depth"] <- object[2, "depth"] + object[2, "thickness"]
  }

  # check for correct height/depth grid:
  if (sum(object$thickness, na.rm = TRUE) > max(object$height)) object[-1, "height"] <- object[-1, "height"] + object[1, "thickness"]
  if (sum(object$thickness, na.rm = TRUE) > max(object$height)) stop("Height grid can't be fixed correctly!")
  if (is.na(object$depth[1])) object$depth[1] <- tail(object$height, 1) - object$height[1]
  # set basal thickness info to NA as a flag
  if (setBasalThicknessNA) object$thickness[1] <- as.double(NA)

  return(object)
}



#' Check whether a profile is observed down to ground or not
#' @param x a [snowprofile], or [snowprofileLayers] object
#' @return boolean TRUE/FALSE
#' @export
hasUnobservedBasalLayer <- function(x) {

  if (is.snowprofile(x)) lyrs <- x$layers
  else if (is.snowprofileLayers(x)) lyrs <- x
  else stop("profile needs to be a snowprofile or snowprofileLayers object!")

  catch <- FALSE
  layerproperties <- colnames(lyrs)

  ## has unobserved basal layer if all properties except following ones are NA
  if (all(is.na(lyrs[1, layerproperties[-which(layerproperties %in% c("height", "depth", "thickness"))]]))) {
    catch <- TRUE

    # if ("hardness" %in% layerproperties) {
    #   if (isTRUE(lyrs$hardness[1] > (0 + 1e-10))) catch <- FALSE  # layer IS observed when hardness larger than 0
    # }
  }  # END if all layers are NA

  return(catch)

}
