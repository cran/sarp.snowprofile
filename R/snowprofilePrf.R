#' Construct snowprofile object from PRF file
#'
#' Read .prf files from SNOWPACK model output
#'
#' @param Filename path to prf file
#' @param ProfileDate read a single profile from file (default = NA will read all profiles)
#' @param tz time zone (default = 'UTC')
#'
#' @return a single snowprofile object of list of multiple snowprofile objects
#'
#' @details
#'
#' Several SNOWPACK model output formats exist see \href{https://models.slf.ch/docserver/snowpack/html/snowpackio.html}{SNOWPACK documentation}
#'
#' Definitions of PRF files are provided at \href{https://models.slf.ch/docserver/snowpack/html/prf_format.html}{https://models.slf.ch/docserver/snowpack/html/prf_format.html}
#'
#' PRF files typically contain profiles from the same station at multiple time steps. If a specific `ProfileDate` is provided a single snowprofile object is returned (search available dates with `scanProfileDates`), otherwise all profiles are read and a list of snowprofile objects is returned.
#'
#' @seealso [snowprofilePro], [scanProfileDates], [snowprofileSno]
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
#' ## Read a single profile by date and plot
#' ProfileDate <- Dates[3]
#' Profile <- snowprofilePrf(Filename, ProfileDate = ProfileDate)
#' plot(Profile)
#'
#' ## Read entire time series and plot
#' Profiles <- snowprofilePrf(Filename)
#' plot(Profiles, main = 'Timeseries read from example.prf')
#'
#' @export
#'
#' @import data.table
#'
snowprofilePrf <- function(Filename,
                           ProfileDate = NA,
                           tz = "UTC") {


  ## ---- Read a single profile ----

  readTabularProfile <- function(TabularProfile) {

    ## Read metadata
    MetaIndex <- which(startsWith(TabularProfile[, 1], "#Date"))
    Metadata <- read.csv(textConnection(TabularProfile[MetaIndex:(MetaIndex + 2), 1]))

    ## Read layer data
    LayerIndex <- which(startsWith(TabularProfile[, 1], "#Full profile") |
                        startsWith(TabularProfile[, 1], "#Aggregated profile"))
    Layers <- read.csv(textConnection(TabularProfile[(LayerIndex + 1):nrow(TabularProfile), 1]))

    ## Drop first rows with units and fix classes of columns
    Metadata <- type.convert(Metadata[-1, ])
    Layers <- type.convert(Layers[-1, ])

    ## Format layers Set NA values
    Layers[Layers == -999] <- NA
    ## Decode Swiss grain types into IACS grain types
    Layers$gtype <- sapply(Layers$class, function(x) ifelse(substr(x, 3, 3) == "2", "MFcr",
                                                            swisscode[as.integer(substr(x, 1, 1))]))
    Layers$gtype <- as.factor(Layers$gtype)
    ## Rename variables and round/format
    Layers$ddate <- as.POSIXct(Layers$X.DepositionDate)
    Layers$height <- round(Layers$Hn, 2)
    Layers$temperature <- round(Layers$Tn, 1)
    Layers$density <- round(Layers$rho, 1)
    Layers$lwc <- round(Layers$theta_w * 100, 1)
    Layers$gsize <- round(Layers$gsz, 2)
    Layers$ogs <- round(Layers$ogs, 2)
    Layers$bond_size <- Layers$bz
    Layers$dendrictiy <- Layers$dd
    Layers$sphericity <- Layers$sp
    Layers$hardness <- round(as.numeric(Layers$hardness), 2)
    if ("ssi" %in% names(Layers)) Layers$ssi <- round(Layers$ssi, 2)
    ## Remove columns
    Layers[c("X.DepositionDate", "DepositionJulianDate", "Hn", "Tn", "gradT", "rho",
             "theta_i", "theta_w", "bz", "dd", "gsz", "sp", "class", "mk")] <- NULL

    ## Create snowprofileLayers object
    Layers <- snowprofileLayers(layerFrame = Layers)

    ## Create snowprofile object
    SP <- snowprofile(station = Metadata$station,
                      station_id = Metadata$station,
                      datetime = as.POSIXct(Metadata$X.Date, format = "%Y-%m-%dT%H:%M:%S"),
                      angle = Metadata$slope,
                      aspect = Metadata$aspect,
                      hs = Metadata$hs,
                      type = "modeled",
                      layers = Layers)

    return(SP)
  }


  ## ---- Read profile ----

  ## Check file extension
  if (!(endsWith(Filename, ".prf") | endsWith(Filename, ".aprf"))) stop(paste(Filename, "needs .prf or .aprf extension"))

  ## Read line (use ~ separator to get single column)
  Lines <- data.table::fread(Filename, sep = "~", data.table = FALSE)

  ## clip all lines that are empty (in the PRF file):
  EmptyLines <- which(nchar(Lines[, ]) == 0)
  Lines <- as.data.frame(Lines[-(EmptyLines), 1], stringsAsFactors = FALSE)

  if (is.na(ProfileDate)) {

    ## Read every profile

    ## Get a unique index for each profile
    DateLines <- which(startsWith(Lines[, 1], "#Date"))
    Nrows <- c(diff(DateLines), nrow(Lines) - DateLines[length(DateLines)] + 1)
    ProfileIndex <- rep(1:length(DateLines), Nrows)

    ## Split data.frame into list of data.frames
    TabularProfiles <- split(Lines, ProfileIndex)

    ## Create list of snowprofiles
    SP <- lapply(TabularProfiles, readTabularProfile)

    ## Convert to class snowprofileSet
    SP <- snowprofileSet(SP)

  } else {

    ## Read profile with specific date

    ## Read existing profile dates profile
    DateLines <- which(startsWith(Lines[, 1], "#Date")) + 2
    Dates <- sapply(Lines[DateLines, ], function(x) unlist(strsplit(x, ","))[1], USE.NAMES = FALSE)
    Dates <- as.POSIXct(Dates, format = "%Y-%m-%dT%H:%M", tz = tz)

    ## Check ProfileDate exists
    if (!(ProfileDate %in% Dates)) stop(paste("No profile with date", ProfileDate, "found in", Filename))

    ## Convert to string for search
    ProfileDate <- format(ProfileDate, format = "%Y-%m-%dT%H:%M", tz = tz)

    ## Find line with ProfileDate and then extent of that profile
    StartLine <- which(startsWith(Lines[, 1], ProfileDate))[1] - 2
    EndLine <- StartLine + 1
    nLines <- nrow(Lines)
    while (!startsWith(Lines[(EndLine + 1), ], "#Date,")) {
      EndLine <- EndLine + 1
      if (EndLine == nLines) break
    }

    ## Copy chunk and create snowprofile object
    TabularProfile <- data.frame(Lines[StartLine:EndLine, ], stringsAsFactors = FALSE)
    SP <- readTabularProfile(TabularProfile)

  }

  ## Return snowprofile or list of snowprofiles
  return(SP)

}
