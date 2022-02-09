#' Pairs of example snowprofiles
#'
#' A list with several entries, each containing a snowprofile object. Pairs of similar profiles are grouped by their names.
#'
#' @docType data
#'
#' @format A list with several entries, that are of class [snowprofile]
#'
#' @keywords snowprofile object
#'
#' @seealso [SPgroup], [SPtimeline]
#'
#' @examples
#' ## Each name refers to one snowprofile:
#' names(SPpairs)
#'
#' opar <- par(no.readonly = TRUE)
#' par(mfrow = c(1, 2))
#' plot(SPpairs$A_manual, main = 'SPpairs$A_manual')
#' plot(SPpairs$A_modeled, main = 'SPpairs$A_modeled')
#' par(opar)
#'
"SPpairs"
