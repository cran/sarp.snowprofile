#' Construct snowprofile object from PRO file
#'
#' Read .pro files from SNOWPACK model output
#'
#' @param Filename path to pro file
#' @param ProfileDate read a single profile from file (default = NA will read all profiles)
#' @param tz time zone (default = 'UTC')
#' @param remove_soil if soil layers are present in PRO file, remove them from snowprofile objects?
#' @param suppressWarnings boolean switch
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
                           tz = "UTC",
                           remove_soil = TRUE,
                           suppressWarnings = FALSE) {

  if (suppressWarnings) {
    old.warn <- options(warn = -1)
    on.exit(options(old.warn))
  }
  ## Function to lookup variable codes in pro files
  codeLookup <- function(Lines, Code) {
    Row <- Lines[which(startsWith(Lines, Code))]
    Row <- unlist(strsplit(Row, ","))
    Row <- Row[3:length(Row)]
    Row <- type.convert(Row, as.is = TRUE)
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

      # Ski pen
      ski_pen <- NA
      if ("0607" %in% Codes)
          ski_pen <- codeLookup(TabularProfile, '0607')  # (m)

      ## Decode layer variables
      height <- codeLookup(TabularProfile, "0501")
      nHeight <- length(height)
      Layers <- data.frame(height = height)
      if ("0502" %in% Codes) {
        density <- codeLookup(TabularProfile, "0502")
        Layers$density <- append(density, rep(NA, times = nHeight - length(density)), after = 0)
      }
      if ("0503" %in% Codes) {
        temperature <- codeLookup(TabularProfile, "0503")
        Layers$temperature <- append(temperature, rep(NA, times = nHeight - length(temperature)), after = 0)
      }
      if ("0506" %in% Codes) {
        lwc <- codeLookup(TabularProfile, "0506")
        Layers$lwc <- append(lwc, rep(NA, times = nHeight - length(lwc)), after = 0)
      }
      if ("0508" %in% Codes) {
        dendrictiy <- codeLookup(TabularProfile, "0508")
        Layers$dendrictiy <- append(dendrictiy, rep(NA, times = nHeight - length(dendrictiy)), after = 0)
      }
      if ("0509" %in% Codes) {
        sphericity <- codeLookup(TabularProfile, "0509")
        Layers$sphericity <- append(sphericity, rep(NA, times = nHeight - length(sphericity)), after = 0)
      }
      if ("0511" %in% Codes) {
        bond_size <- codeLookup(TabularProfile, "0511")
        Layers$bond_size <- append(bond_size, rep(NA, times = nHeight - length(bond_size)), after = 0)
      }
      if ("0512" %in% Codes) {
        gsize <- codeLookup(TabularProfile, "0512")
        Layers$gsize <- append(gsize, rep(NA, times = nHeight - length(gsize)), after = 0)
      }
      if ("0523" %in% Codes) {
        v_strain_rate <- codeLookup(TabularProfile, "0523")
        Layers$v_strain_rate <- append(v_strain_rate, rep(NA, times = nHeight - length(v_strain_rate)), after = 0)
      }
      if ("0533" %in% Codes) {
        sk38 <- codeLookup(TabularProfile, "0533")
        Layers$sk38 <- append(sk38, rep(NA, times = nHeight - length(sk38)), after = 0)
      }
      if ("0534" %in% Codes) {
        hardness <- -codeLookup(TabularProfile, "0534")
        Layers$hardness <- append(hardness, rep(NA, times = nHeight - length(hardness)), after = 0)
      }
      if ("0535" %in% Codes) {
        ogs <- codeLookup(TabularProfile, "0535")
        Layers$ogs <- append(ogs, rep(NA, times = nHeight - length(ogs)), after = 0)
      }
      if ("0540" %in% Codes) {
        ddate <- codeLookup(TabularProfile, "0540")
        Layers$ddate <- append(ddate, rep(NA, times = nHeight - length(ddate)), after = 0)
        Layers$ddate <- as.POSIXct(Layers$ddate, format = "%d.%m.%Y %H:%M", tz = tz)
      }
      if ("0601" %in% Codes) {
        shear_strength <- codeLookup(TabularProfile, "0601")
        Layers$shear_strength <- append(shear_strength, rep(NA, times = nHeight - length(shear_strength)), after = 0)
      }
      if ("0604" %in% Codes) {
        ssi <- codeLookup(TabularProfile, "0604")
        Layers$ssi <- append(ssi, rep(NA, times = nHeight - length(ssi)), after = 0)
      }
      if ("0606" %in% Codes) {
        ccl <- codeLookup(TabularProfile, "0606")
        Layers$ccl <- append(ccl, rep(NA, times = nHeight - length(ccl)), after = 0)
      }

      ## Decode grain class
      if ("0513" %in% Codes) {
        gclass <- codeLookup(TabularProfile, "0513")
        gclass <- gclass[1:(length(gclass) - 1)]
        gtype <- sapply(gclass, function(x) ifelse(substr(x, 3, 3) == "2", "MFcr",
                                                   swisscode[as.integer(substr(x, 1, 1))]))
        Layers$gtype <- as.factor(append(gtype, rep(NA, times = nHeight - length(gtype)), after = 0))
      }

      ## remove soil_layers
      if (remove_soil) {
        Layers <- Layers[Layers$height > 0, ]
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

      if (!is.na(ski_pen)) SP$ski_pen <- ski_pen

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

  if (suppressWarnings) options(old.warn)
  ## Return snowprofile or list of snowprofiles
  return(SP)

}
