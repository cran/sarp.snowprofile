#' Conversion of Hand Hardness Index (HHI)
#'
#' Convert character hand hardness index (HHI) of snow layers to numeric values.
#' For example, hand hardness Fist becomes 1, Ice becomes 6.
#'
#' @param charHHI Character string of hand hardness level, i.e., one of
#'   - Fist 'F', 4 Fingers '4F', 1 Finger '1F', Pencil 'P', Knife 'K', or Ice 'I'
#'   - intermediate values allowed, e.g. 'F+', '1F-', 'F-4F'
#'
#' @return Float value of numeric hand hardness level between 1 and 6.
#'
#' @author fherla
#'
#' @examples
#' char2numHHI('F+')
#' char2numHHI('F-')
#' char2numHHI('F-4F')
#'
#' ## not meaningful:
#' this_throws_error <- TRUE
#' if (!this_throws_error) {
#' char2numHHI('F-P')
#' }
#'
#' @export
#'
char2numHHI <- function(charHHI) {

  if (is.numeric(charHHI))
    return(charHHI)

  ## Assign numeric values to each element of hand hardness index
  characterHHI <- c("F", "4F", "1F", "P", "K", "I", "+", "-")
  variation <- list(`+` = +0.25, `-` = -0.25)
  transMat <- data.frame(numericHHI = c(c(1:6), -0.25, 0.25),
                         row.names = characterHHI)

  ## extract hand hardness levels, while considering mix forms, such as:
  ## 'F-4F' or '4F+' or '4F-'
  chars <- c(NA, NA)
  var <- NA
  if (grepl(pattern = "-$", x = as.character(charHHI))) {
    chars[1] <- strsplit(as.character(charHHI), "-")[[1]]
    var <- variation$`-`
  } else if (grepl(pattern = "[+]$", x = as.character(charHHI))) {
    chars[1] <- strsplit(as.character(charHHI), "+")[[1]][1]
    var <- variation$`+`
  } else if (grepl(pattern = "-", x = as.character(charHHI))) {
    chars <- strsplit(as.character(charHHI), "-")[[1]]
  } else {
    chars[1] <- as.character(charHHI)
  }
  nums <- c(transMat[chars[[1]], "numericHHI"],
            transMat[chars[[2]], "numericHHI"])

  ## Error Handling for non-neighboring levels, such as '1F-P'
  if (!any(is.na(nums))) {
    if (diff(nums) > 1) {
      stop(paste0("Non-neighboring hardness levels ",
                  nums[1], " and ", nums[2], " (in '", charHHI, "')"))
    }
  }

  ## calculate numeric HHI from mean of 1 or 2 layers and potential variation:
  numHHI <- mean(nums, na.rm = TRUE) + sum(var, na.rm = TRUE)
  return(numHHI)
}
