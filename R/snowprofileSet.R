
#' Constructor for class snowprofileSet
#'
#' @param x list of [snowprofile] objects
#'
#' @return a snowprofileSet
#'
#' @seealso [snowprofile], [summary.snowprofileSet]
#'
#' @export
#'
snowprofileSet <- function(x = list()) {

  ## Check all list entries are class snowprofile
  if (!inherits(x, 'list')) stop('x must be a list')
  if (!all(sapply(x, inherits, 'snowprofile'))) stop("Each value in the list must be of class 'snowprofile'")

  ## Assign class if it passses tests
  class(x) <- append("snowprofileSet", class(x))
  return(x)

}


#' Check class snowprofileSet
#'
#' Check if object is of class [snowprofileSet]
#'
#' @param x object to test
#'
#' @return boolean
#'
#' @export
is.snowprofileSet <- function(x) inherits(x, "snowprofileSet")



#' Extract method
#'
#' @param x object from which to extract element(s) or in which to replace element(s).
#' @param i indices specifying elements to extract or replace
#'
#' @return [snowprofileSet] object
#' @export
#'
`[.snowprofileSet` <- function(x, i) {
  snowprofileSet(NextMethod())
}
