#' Write a SMET file
#'
#' Write data into a SMET file https://models.slf.ch/docserver/meteoio/SMET_specifications.pdf
#'
#' @import data.table
#'
#' @param smet A data structure that resembles a smet file (i.e., list containing metadata and a
#' data.frame, see example in [readSmet])
#' @param filename Filepath to be written
#'
#' @return Generates smet file
#' @export
#'
#' @author fherla, shorton
#'
#' @seealso [readSmet], [snowprofileSno], [snowprofilePrf], [snowprofilePro]
#'
#' @examples
#' ## First read example smet file provided in package
#' (Wx = readSmet(system.file('extdata', 'example.smet', package = 'sarp.snowprofile')))
#'
#' ## Then write Wx to a new temp file and show the file
#' writeSmet(Wx, filename = file.path(tempdir(), 'file.smet'))
#' file.show(file.path(tempdir(), 'file.smet'))
#'
#' ## Check whether it can be read back in
#' (WxNew <- readSmet(file.path(tempdir(), 'file.smet')))
#'
writeSmet <- function(smet, filename) {

  ## --- assertions ----
  if (! "data" %in% names(smet)) warning("You're writing a smet file with empty DATA. Consider providing smet$data.")


  ## --- manipulate key value pairs ----
  smet$fields <- paste(smet$fields, collapse = " ")  # to avoid printing it like an R array of strings

  ## prepare HEADER and DATA sections including string paddings for readability
  smet_names <- names(smet)
  smet_names_pad <- smet_names[smet_names != "data"]
  smet_names_pad[smet_names_pad != "fields"] <- stringr::str_pad(smet_names_pad[smet_names_pad != "fields"], 16, side = "right")

  x <- list(HEADER=smet[smet_names != "data"], DATA=smet$data)
  names(x$HEADER) <- smet_names_pad

  ## --- writing ---
  sink(filename, type = "output")

  writeLines("SMET 1.1 ASCII")

  for (section in names(x)) {
    writeLines(paste0("[", section, "]"))
    if (section == "DATA") break
    for (key in x[section]) {
      writeLines(paste0(names(key), " = ", key))
    }
  }
  sink()
  data.table::fwrite(x$DATA, file = filename, append = TRUE, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE,
                     dateTimeAs = "ISO", na = as.character(x$HEADER$`nodata           `))



}



