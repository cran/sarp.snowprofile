#' Read profile dates from prf/pro file
#'
#' Before reading entire SNOWPACK output it can be helpful to  scan the profile timestamps first
#'
#' @param Filename filename
#' @param tz time zone (default = 'UTC')
#'
#' @return vector of as.POSIXct timestamps
#'
#' @seealso [snowprofilePrf], [snowprofilePro]
#'
#' @author shorton
#'
#' @examples
#'
#' ## Path to example prf file
#' Filename <- system.file('extdata', 'example.prf', package = 'sarp.snowprofile')
#'
#' ## Scan dates in file
#' Dates <- scanProfileDates(Filename)
#' print(Dates)
#'
#' @export
#'
#' @import data.table
#'
scanProfileDates <- function(Filename, tz = "UTC") {


  ## Read lines (use ~ separator to get single column)
  Lines <- data.table::fread(Filename, sep = "~", data.table = FALSE)

  ## Initialize empty dates list
  Dates <- c()

  ## Parse dates from pro file
  if (endsWith(Filename, ".pro")) {
    DateLines <- which(startsWith(Lines[, 1], "0500"))
    DateLines <- DateLines[-1]
    Dates <- gsub("0500,", "", Lines[DateLines, ])
    Dates <- as.POSIXct(Dates, format = "%d.%m.%Y %H:%M", tz = tz)

  ## Parse dates from prf file
  } else if (endsWith(Filename, ".prf") | endsWith(Filename, ".aprf")) {
    DateLines <- which(startsWith(Lines[, 1], "#Date")) + 2
    Dates <- sapply(Lines[DateLines, ], function(x) unlist(strsplit(x, ","))[1], USE.NAMES = FALSE)
    Dates <- as.POSIXct(Dates, format = "%Y-%m-%dT%H:%M", tz = tz)

  ## Warning for incorrect file format
  } else {
    warning(paste("Filename must have '.prf' '.aprf' or '.pro' extension", Filename))
  }

  ## Return list of dates
  return(Dates)

}
