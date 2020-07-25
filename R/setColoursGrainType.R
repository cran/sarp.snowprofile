#' Set colour scale for grain types
#'
#' Currently, you can choose between 'iacs', 'iacs2', 'sarp', or 'sarp-reduced'.
#'
#' @param ScaleName Name of graintype colour scale
#'
#' - `iacs:` scale defined by the *International Classification of Seasonal Snow on the Ground*
#' - `iacs2:` scale defined by the *International Classification of Seasonal Snow on the Ground* with a dark red colour for MFcr layers so that MF and MFcr layers can be better distinguished.
#' - `sarp:` hazard adjusted colours for grain types based on Horton et al. (2020)
#' - `sarp-reduced:` hazard adjusted colours for groups of grain types based on Horton et al. (2020)
#'
#' @return data.frame containing the new colour values stored in `grainDict`
#'
#' @seealso [grainDict], [getColoursGrainType]
#'
#' @references
#'
#' Horton, S., Nowak, S., and Haegeli, P.: Enhancing the operational value of snowpack models with visualization design principles,
#' Nat. Hazards Earth Syst. Sci., 20, 1557–1572, \href{https://doi.org/10.5194/nhess-20-1557-2020}{https://doi.org/10.5194/nhess-20-1557-2020}, 2020.
#'
#' @examples
#'
#' ## Current/default grain type colours
#' grainDict
#' plot(SPpairs$A_manual, main = 'Snow profile with default colours')
#'
#' ## Change to IACS colours
#' grainDict <- setColoursGrainType('IACS')
#' grainDict
#' plot(SPpairs$A_manual, main = 'Snow profile with IACS colours')
#'
#' ## Change to IACS colours with adjusted MFcr (darkred)
#' grainDict <- setColoursGrainType('IACS2')
#' grainDict
#' plot(SPpairs$A_manual, main = 'Snow profile with IACS colours and adjusted darkred MFcr')
#'
#' ## Change to SARP colours
#' grainDict <- setColoursGrainType('SARP')
#' grainDict
#' plot(SPpairs$A_manual, main = 'Snow profile with SARP colours')
#'
#' ## Change to reduced SARP colours
#' grainDict <- setColoursGrainType('SARP-reduced')
#' grainDict
#' plot(SPpairs$A_manual, main = 'Snow profile with a reduced set of SARP colours')
#'
#' @export
#'
setColoursGrainType <- function(ScaleName) {

  ScaleName <- tolower(ScaleName)

  ## IACS scale
  if (ScaleName == "iacs") {

    grainDict <- data.frame(gtype = c("PP", "DF", "RG", "FC", "DH", "SH", "MF", "IF", "PPgp", "FCxr", "MFcr"),
                            colour = c("#00FF00", "#228B22", "#FFB6C1", "#ADD8E6", "#0000FF", "#FF00FF", "#FF0000", "#00FFFF", "#00FF00", "#ADD8E6", "#FF0000"),
                            stringsAsFactors = FALSE)

    message(paste0("The grain type colour scale has been set to '", ScaleName, "'.\n"))
    return(grainDict)

  ## IACS scale with dark red MFcr
  } else if (ScaleName == "iacs2") {

    grainDict <- data.frame(gtype = c("PP", "DF", "RG", "FC", "DH", "SH", "MF", "IF", "PPgp", "FCxr", "MFcr"),
                            colour = c("#00FF00", "#228B22", "#FFB6C1", "#ADD8E6", "#0000FF", "#FF00FF", "#FF0000", "#00FFFF", "#00FF00", "#ADD8E6", "#961111"),
                            stringsAsFactors = FALSE)

    message(paste0("The grain type colour scale has been set to '", ScaleName, "'.\n"))
    return(grainDict)

  ## SARP scale
  } else if (ScaleName == "sarp") {

    grainDict <- data.frame(gtype = c("SH", "DH", "PP", "DF", "RG", "FCxr", "FC", "MFcr", "MF", "IF"),
                            colour = c("#ee3a1d", "#4678e8", "#ffde00", "#f1f501", "#ffccd9", "#dacef4", "#b2edff", "#addd8e", "#d5ebb5", "#a3ddbb"),
                            stringsAsFactors = FALSE)

    message(paste0("The grain type colour scale has been set to '", ScaleName, "'.\n"))
    return(grainDict)

  ## SARP scale reduced
  } else if (ScaleName == "sarp-reduced") {

    grainDict <- data.frame(gtype = c("SH", "DH", "PP", "DF", "RG", "FCxr", "FC", "MFcr", "MF", "IF"),
                            colour = c("#95258f", "#95258f", "#ffde00", "#ffde00", "#dacef4", "#dacef4", "#dacef4", "#d5ebb5", "#d5ebb5", "#d5ebb5"),
                            stringsAsFactors = FALSE)

    message(paste0("The grain type colour scale has been set to '", ScaleName, "'.\n"))
    return(grainDict)

  } else {

    message(paste0("The grain type colour scale '", ScaleName, "' has not been defined!\n"))

  }

}
