#' Construct snowprofile object from PRO file
#'
#' Read .pro files from SNOWPACK model output
#'
#' @param Filename path to pro file
#' @param ProfileDate read a single profile from file (default = NA will read all profiles)
#' @param tz time zone (default = 'UTC')
#'
#' @return a single snowprofile object of list of multiple snowprofile objects
#'
#' @details
#'
#' Several SNOWPACK model output formats exist see \href{https://models.slf.ch/docserver/snowpack/html/snowpackio.html}{SNOWPACK documentation}
#'
#' Definitions of PRO files are provided at \href{https://models.slf.ch/docserver/snowpack/html/pro_format.html}{https://models.slf.ch/docserver/snowpack/html/pro_format.html} and an example file is available at \href{https://run.niviz.org/?file=resources%2Fexample.pro}{niViz}
#'
#' PRO files typically contain profiles from the same station at multiple time steps. If a specific `ProfileDate` is provided a single snowprofile object is returned (search available dates with `scanProfileDates`), otherwise all profiles are read and a list of snowprofile objects is returned.
#'
#' @seealso [snowprofilePrf], [scanProfileDates], [snowprofileSno]
#'
#' @author shorton
#'
#' @examples
#'
#' ## Path to example pro file
#' Filename <- system.file('extdata', 'example.pro', package = 'sarp.snowprofile')
#'
#' ## Download example pro file from niViz
#' #Filename <- tempfile(fileext = '.pro')
#' #download.file('https://niviz.org/resources/example.pro', Filename)
#'
#' ## Scan dates in file
#' Dates <- scanProfileDates(Filename)
#' print(Dates)
#'
#' ## Read a single profile by date and plot
#' ProfileDate <- Dates[3]
#' Profile <- snowprofilePro(Filename, ProfileDate = ProfileDate)
#' plot(Profile)
#'
#' ## Read entire time series and plot
#' Profiles <- snowprofilePro(Filename)
#' plot(Profiles, main = 'Timeseries read from example.pro')
#'
#' @export
#'
#' @import data.table
#'
snowprofilePro <- function(Filename,
                           ProfileDate = NA,
                           tz = "UTC") {

  ## Function to lookup variable codes in pro files
  codeLookup <- function(Lines, Code) {
    Row <- Lines[which(startsWith(Lines, Code))]
    Row <- unlist(strsplit(Row, ","))
    Row <- Row[3:length(Row)]
    Row <- type.convert(Row)
    return(Row)
  }

  ## ---- Read a single profile ----

  readTabularProfile <- function(TabularProfile, StationData) {

    ## Convert to vector
    TabularProfile <- TabularProfile[, 1]

    ## Which variables exist layer variables
    Codes <- sapply(strsplit(TabularProfile, ","), "[", 1)

    ## Return nothing if there's no layers
    if (length(codeLookup(TabularProfile, "0501")) == 1) {
      return(NULL)

    } else {

      # Datetime
      datetime <- codeLookup(TabularProfile, "0500")[2]
      datetime <- as.POSIXct(datetime, format = "%d.%m.%Y %H:%M", tz = tz)

      ## Decode layer variables
      Layers <- data.frame(height = codeLookup(TabularProfile, "0501"))
      if ("0502" %in% Codes)
          Layers$density <- codeLookup(TabularProfile, "0502")
      if ("0503" %in% Codes)
          Layers$temperature <- codeLookup(TabularProfile, "0503")
      if ("0506" %in% Codes)
          Layers$lwc <- codeLookup(TabularProfile, "0506")
      if ("0508" %in% Codes)
          Layers$dendrictiy <- codeLookup(TabularProfile, "0508")
      if ("0509" %in% Codes)
          Layers$sphericity <- codeLookup(TabularProfile, "0509")
      if ("0511" %in% Codes)
          Layers$bond_size <- codeLookup(TabularProfile, "0511")
      if ("0512" %in% Codes)
          Layers$gsize <- codeLookup(TabularProfile, "0512")
      if ("0534" %in% Codes)
          Layers$hardness <- -codeLookup(TabularProfile, "0534")
      if ("0535" %in% Codes)
          Layers$ogs <- codeLookup(TabularProfile, "0535")
      if ("0604" %in% Codes)
          Layers$ssi <- codeLookup(TabularProfile, "0604")
      if ("0606" %in% Codes)
          Layers$ccl <- codeLookup(TabularProfile, "0606")

      ## Decode grain class
      if ("0513" %in% Codes) {
        gclass <- codeLookup(TabularProfile, "0513")
        gclass <- gclass[1:(length(gclass) - 1)]
        Layers$gtype <- sapply(gclass, function(x) ifelse(substr(x, 3, 3) == "2", "MFcr",
                                                          swisscode[as.integer(substr(x, 1, 1))]))
        Layers$gtype <- as.factor(Layers$gtype)
      }

      ## Set NA values
      Layers[Layers == -999] <- NA

      ## Create snowprofileLayers object
      Layers <- snowprofileLayers(layerFrame = Layers)

      ## Create snowprofile object
      SP <- snowprofile(station = StationData$StationName,
                        station_id = StationData$StationName,
                        datetime = datetime,
                        latlon = as.numeric(c(StationData$Latitude, StationData$Longitude)),
                        elev = as.numeric(StationData$Altitude),
                        angle = as.numeric(StationData$SlopeAngle),
                        aspect = as.numeric(StationData$SlopeAzi),
                        type = "modeled",
                        layers = Layers)

      return(SP)
    }

  }


  ## ---- Read file ----

  ## Check file extension
  if (!endsWith(Filename, ".pro")) stop(paste(Filename, "needs .pro extension"))

  ## Read lines (use ~ separator to get single column)
  Lines <- data.table::fread(Filename, sep = "~", data.table = FALSE)

  ## Determine index of Sections
  HeaderStart <- which(Lines == "[HEADER]")
  DataStart <- which(Lines == "[DATA]")

  ## Parse station info
  StationData <- list()
  Header <- Lines[1:(HeaderStart - 1), 1]
  for (Row in Header[Header != ""]) {
    KeyValue <- unlist(strsplit(gsub(" ", "", Row), "="))
    StationData[[KeyValue[1]]] <- KeyValue[2]
  }

  ## Keep data rows
  Lines <- data.frame(Lines[(DataStart + 1):nrow(Lines), ], stringsAsFactors = FALSE)


  if (is.na(ProfileDate)) {

    ## ---- Read every profile ----

    ## Get a unique index for each profile
    DateLines <- which(startsWith(Lines[, 1], "0500"))
    Nrows <- c(diff(DateLines), nrow(Lines) - DateLines[length(DateLines)] + 1)
    ProfileIndex <- rep(1:length(DateLines), Nrows)

    ## Split data.frame into list of data.frames
    TabularProfiles <- split(Lines, ProfileIndex)

    ## Create list of snowprofiles
    SP <- lapply(TabularProfiles, readTabularProfile, StationData = StationData)

    ## Remove empty elements
    SP <- SP[!sapply(SP, is.null)]

    ## Convert to class snowprofileSet
    SP <- snowprofileSet(SP)

  } else {

    ## ---- Read profile with specific date ----

    ## Read existing profile dates profile
    DateLines <- which(startsWith(Lines[, 1], "0500"))
    Dates <- na.omit(gsub("0500,", "", Lines[DateLines, ]))
    Dates <- as.POSIXct(Dates, format = "%d.%m.%Y %H:%M", tz = tz)

    ## Check ProfileDate exists
    if (!(ProfileDate %in% Dates)) stop(paste("No profile with date", ProfileDate, "found in", Filename))

    ## Find line with ProfileDate and then extent of that profile
    StartLine <- DateLines[which(Dates == ProfileDate)]
    EndLine <- StartLine
    nLines <- nrow(Lines)
    while (!startsWith(Lines[(EndLine + 1), ], "0500")) {
      EndLine <- EndLine + 1
      if (EndLine == nLines) break
    }

    ## Copy chunk and create snowprofile object
    TabularProfile <- data.frame(Lines[StartLine:EndLine, ], stringsAsFactors = FALSE)
    SP <- readTabularProfile(TabularProfile, StationData)

  }

  ## Return snowprofile or list of snowprofiles
  return(SP)

}
