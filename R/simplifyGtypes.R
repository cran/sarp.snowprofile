#' Simplify detailed grain types to parent classes
#'
#' The IACS records grain types in major and minor classes, e.g. precipitation particles PP can be subclassified into
#' stellar dendrites PPsd. Some of these subclasses are not supported in this R package and so this function simplifies
#' the unsupported gran type subclasses into their supported main classes. If a given grain type cannot be simplified,
#' a NA value is returned for it.
#'
#' @param gtypes an array of character grain types following IACS standards
#' @param supported_gtypes an array of supported grain types that will determine the simplification
#' @return the modified input array
#'
#' @author fherla
#'
#' @examples
#' ## create an array of gtypes
#' gtypes <- c('FCxr', 'RGxf', 'PPsd', 'PP', 'IFrc', "KKfx")
#'
#' ## sinplify gtypes to supported_gtypes:
#' simplifyGtypes(gtypes)
#'
#' @export
simplifyGtypes <- function(gtypes, supported_gtypes = grainDict$gtype) {

  supported_gtypes <- unique(c(supported_gtypes, NA))
  id_unknown <- which(!gtypes %in% supported_gtypes)

  ## all gtypes are known
  if (length(id_unknown) == 0) return(gtypes)

  ## need to simplify:
  gtypes_unknown <- gtypes[id_unknown]
  gtypes_unknown_trim <- strtrim(gtypes_unknown, 2)
  gtypes_mod <- ifelse(gtypes_unknown_trim %in% supported_gtypes, gtypes_unknown_trim, NA)

  ## merge and return
  gtypes[id_unknown] <- gtypes_mod
  return(gtypes)
}
