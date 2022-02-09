#' Construct snowprofile object from SNO file
#'
#' Read .sno files from SNOWPACK model input/output
#'
#' @param Filename path to sno file
#'
#' @return a [snowprofile] object
#'
#' @details
#'
#' Several SNOWPACK model output formats exist see \href{https://models.slf.ch/docserver/snowpack/html/snowpackio.html}{SNOWPACK documentation}
#'
#' Definitions of SNO files are provided at \href{https://models.slf.ch/docserver/snowpack/html/smet.html}{https://models.slf.ch/docserver/snowpack/html/smet.html}
#'
#' @seealso [snowprofilePro], [snowprofilePrf], [snowprofileCsv]
#'
#' @author shorton
#'
#' @examples
#'
#' ## Path to example prf file
#' Filename <- system.file('extdata', 'example.sno', package = 'sarp.snowprofile')
#'
#' ## Read snowprofile object
#' Profile <- snowprofileSno(Filename)
#'
#' ## Note: plot.snowprofile won't work because sno files don't have harndess
#'
#' ## Plot a temperautre profile
#' plot(snowprofileSet(list(Profile)), ColParam = 'temp')
#'
#' @export
#'
snowprofileSno <- function(Filename) {

  ## Parse smet file
  Smet <- readSmet(Filename)

  ## Create snowprofileLayers object
  HS <- Smet$HS_Last * 100
  Smet$data$height <- cumsum(Smet$data$Layer_Thick) * 100
  names(Smet$data)[names(Smet$data) == "timestamp"] <- "ddate"
  Layers <- snowprofileLayers(layerFrame = Smet$data, hs = HS)

  ## Rename variables and round/format
  Layers$temperature <- round(Layers$T - 273.15, 1)
  Layers$gsize <- round(Layers$rg, 2)
  Layers$bond_size <- Layers$rb
  Layers$dendrictiy <- Layers$dd
  Layers$sphericity <- Layers$sp
  ## Remove columns
  Layers[c("Layer_Thick", "T", "rg", "rb", "dd", "sp")] <- NULL

  ## Create snowprofile object
  SP <- snowprofile(station = Smet$station_name,
                    station_id = Smet$station_id,
                    datetime = as.POSIXct(Smet$ProfileDate, format = "%Y-%m-%dT%H:%M:%S"),
                    latlon = c(Smet$latitude, Smet$longitude),
                    elev = Smet$altitude,
                    angle = Smet$slope_angle,
                    aspect = Smet$slope_azi,
                    hs = HS,
                    type = "vstation",
                    layers = Layers)

  ## Return snowprofile object
  return(SP)

}
