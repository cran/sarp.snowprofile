#' Gets colours for plotting snow stability indices
#'
#' Gets colours for plotting snow stability indices in snowprofiles.
#'
#'
#' @param Values Stability index values
#' @param StabilityIndexThreshold A scalar threshold that defines the transition from medium to poor stability.
#' The color scheme will be adjusted so that this threshold becomes apparent from the colours.
#' @param StabilityIndexRange The range the index spans, e.g. for TSA `[0, 6]`, for RTA and p_unstable `[0, 1]`, for critical crack length `[0, 3]`, etc..
#' @param invers Indices like TSA/ RTA/ p_unstable increase the poorer layer stability gets. For indices with revers behaviour (e.g.,, critical crack length) switch this flag to `TRUE`.
#' @param Resolution Resolution of colour scale. Default is 100.
#'
#' @return Array with HTML colour codes
#'
#' @author fherla
#'
#' @seealso [getColoursGrainSize], [getColoursGrainType], [getColoursHardness], [getColoursLWC], [getColoursSnowTemp], [getColoursPercentage]
#'
#' @examples
#'
#' p_unstable <- seq(0, 1, by=0.1)
#' plot(x = rep(1,length(p_unstable)), y = p_unstable,
#'      col = getColoursStability(p_unstable), pch = 19, cex = 3)
#'
#' critical_crack_length <- c(seq(0.2, 0.8, by=0.1), 1.5, 2.5)
#' plot(x = rep(1,length(critical_crack_length)), y = critical_crack_length, pch = 19, cex = 3,
#'      col = getColoursStability(critical_crack_length, StabilityIndexThreshold = 0.4,
#'                                StabilityIndexRange = c(0, 3), invers = TRUE))
#'
#' @export
#'
getColoursStability <- function(Values, StabilityIndexThreshold = 0.77, StabilityIndexRange = c(0, 1), invers = FALSE, Resolution = 100) {

  if (!invers) {
    StabilityIndexRange[2] <- StabilityIndexRange[2] + 0.01* StabilityIndexRange[2]
    goodPalette <- colorRampPalette(c('#FFFFB2', '#FED976', '#FEB24C'))
    poorPalette <- colorRampPalette(c('#FC4E2A', '#E31A1C', '#B10026'))

    cutval <- cut(Values, breaks = unique(c(seq(StabilityIndexRange[1], StabilityIndexThreshold, len = floor((Resolution+2)* StabilityIndexThreshold/StabilityIndexRange[2])),
                                            seq(StabilityIndexThreshold, StabilityIndexRange[2], len = ceiling((Resolution+2)* (1-StabilityIndexThreshold/StabilityIndexRange[2]))))),
                  right = FALSE)

    labs <- levels(cutval)
    poorResolutionVec <- which(as.numeric( sub("[^,]*,([^]]*)\\)", "\\1", labs)) > StabilityIndexThreshold)
    poorIdx <- which(as.numeric(cutval) >= min(poorResolutionVec))
    goodIdx <- which(as.numeric(cutval) < min(poorResolutionVec))

    poorCols <- poorPalette(length(poorResolutionVec))
    goodCols <- goodPalette(length(labs) - length(poorResolutionVec))

    Cols <- rep("#d9d9d9", length(Values))  # initialize with gray col
    Cols[goodIdx] <- goodCols[as.numeric(cutval[goodIdx])]
    Cols[poorIdx] <- poorCols[as.numeric(cutval[poorIdx])-min(poorResolutionVec)+1]

  ## invers:
  } else {
    StabilityIndexRange[1] <- StabilityIndexRange[1] - max(0.01* StabilityIndexRange[1], 0.01)
    goodPalette <- colorRampPalette(rev(c('#FFFFB2', '#FED976', '#FEB24C')))
    poorPalette <- colorRampPalette(rev(c('#FC4E2A', '#E31A1C', '#B10026')))

    cutval <- cut(Values, breaks = unique(c(seq(StabilityIndexRange[1], StabilityIndexThreshold, len = ceiling((Resolution+2)* StabilityIndexThreshold/StabilityIndexRange[2])),
                                            seq(StabilityIndexThreshold, StabilityIndexRange[2], len = floor((Resolution+2)* (1-StabilityIndexThreshold/StabilityIndexRange[2]))))),
                  right = TRUE)

    labs <- levels(cutval)
    poorResolutionVec <- which(as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs)) <= StabilityIndexThreshold)
    poorIdx <- which(as.numeric(cutval) <= max(poorResolutionVec))
    goodIdx <- which(as.numeric(cutval) > max(poorResolutionVec))

    poorCols <- poorPalette(length(poorResolutionVec))
    goodCols <- goodPalette(length(labs) - length(poorResolutionVec))

    Cols <- rep("#d9d9d9", length(Values))  # initialize with gray col
    Cols[goodIdx] <- goodCols[as.numeric(cutval[goodIdx])]
    Cols[poorIdx] <- poorCols[as.numeric(cutval[poorIdx])-min(poorResolutionVec)+1]

  }


  return(Cols)

}
