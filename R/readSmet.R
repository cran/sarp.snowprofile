#' Parse a SMET file
#'
#' Read contents of a SMET file https://models.slf.ch/docserver/meteoio/SMET_specifications.pdf
#'
#' @import data.table
#'
#' @param Filename Path to a smet file
#'
#' @return List containing metadata and data
#' @export
#'
#' @author shorton
#'
#' @seealso [snowprofileSno], [snowprofilePrf], [snowprofilePro]
#'
#' @examples
#'
#' ## Path to example smet
#' Filename <- system.file('extdata', 'example.smet', package = 'sarp.snowprofile')
#' Wx = readSmet(Filename)
#' str(Wx)
#'
readSmet <- function(Filename) {

  ## ---- Read profile ----

  ## Check file extension
  SmetExt <- c(".smet", ".sno")
  if (!any(endsWith(Filename, SmetExt))) stop(paste(Filename, "needs extension:", paste(SmetExt, collapse = " ")))

  ## Initialize list to hold data
  Wx <- list()

  ## Read line (use ~ separator to get single column)
  Lines <- data.table::fread(Filename, sep = "~", data.table = FALSE)
  Lines <- Lines[, 1]

  ## ---- Parse contents ----

  ## Determine rows where sections start
  HeaderIndex <- which(startsWith(Lines, "[HEADER]"))
  DataIndex <- which(startsWith(Lines, "[DATA]"))

  ## Read header into a list
  Header <- Lines[(HeaderIndex + 1):(DataIndex - 1)]
  Header <- Header[Header != ""]
  for (Row in Header) {
    KeyValue <- unlist(strsplit(Row, "="))
    Key <- gsub(" ", "", KeyValue[1])
    Value <- KeyValue[2]
    if (Key %in% c("fields", "units_offset", "units_multiplier",
                   "plot_unit", "plot_description", "plot_color")) {
        Value <- unlist(strsplit(Value, " "))
        Value <- Value[Value != ""]
    } else {
        Value <- gsub(" ", "", Value)
    }
    Wx[[Key]] <- type.convert(Value, as.is = TRUE)
  }

  ## Read data
  Data <- read.csv(textConnection(Lines[(DataIndex + 1):length(Lines)]), sep = "")
  stopifnot(ncol(Data) == length(Wx$fields))
  names(Data) <- Wx$fields


  ## ---- Format data ----

  ## Default converstions
  Data <- type.convert(Data, as.is = TRUE)

  ## Set NA value
  if ("nodata" %in% names(Wx)) Data[Data == as.numeric(Wx$nodata)] <- NA

  ## Format timestamp
  if ("timestamp" %in% names(Data)) {
    tz <- ifelse("tz" %in% names(Wx), paste0("Etc/GMT", ifelse(Wx$tz >= 0, "+", ""), Wx$tz), "GMT")
    Timestamp <- as.POSIXct(Data$timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = tz)
    if (any(is.na(Timestamp))) {
      WithoutSeconds <- as.POSIXct(Data$timestamp, format = "%Y-%m-%dT%H:%M", tz = tz)
      if (!any(is.na(WithoutSeconds)))
        Timestamp <- WithoutSeconds
    }
    Data$timestamp <- Timestamp
  }

  ## Add data to list
  Wx$data <- Data

  ## Return list
  return(Wx)

}
