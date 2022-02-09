## This utils file contains useful, but brief helper functions.

#' fast uncorrected sample standard deviation
#' https://en.wikipedia.org/wiki/Standard_deviation#Rapid_calculation_methods
#'
#' @param x a numeric vector
#' @param xbar arithmetic mean of x
#' @param na.rm remove any NAs before computation of standard deviation?
#' @return uncorrected sample standard deviation (i.e., a numeric scalar)
#' @author fherla
#' @export
  sd_sample_uncorrected <- function(x, xbar = mean(x), na.rm = FALSE) {
    if (na.rm) x <- na.omit(x)
    n <- length(x)
    sqrt(1/n * sum((x-xbar)**2))
}
