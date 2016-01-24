#' Read trapping data
#'
#' \code{read.trapping} reads data from an excel file and returns a
#' \code{data.frame} after having conducted basic quality checks
#'
#' Default file name is "Woodland Reserve Fauna Data.xlsx", but this can be set
#' to NULL and an interactive window will open to browse to the file. In this
#' case, \code{dir.in} is taken from the path of the file selected.
#'
#' If \code{dir.in} is NULL and \code{nfile} is provided, then an interactive
#' window is opened to select the path to \code{nfile}.
#'
#' If \code{last.row} is left to default (0), a warning is reported. This is
#' because currently the excel file uses several formula to validate/obtain data
#' from the IDs list. If validated fields/formula are present in the last lines
#' (even if no data is actually inserted) these are read as non-empty rows in R
#' and filled with NA, potentially causing problem for downstream analyses.
#'
#' @param dir.in The path to the folder where the data file is located
#' @param nfile The name of the (excel) data file
#' @param sheet The name of the sheet to be imported
#' @param last.row An integer with the number of the last row to be imported. If
#'   zero (default), the last row is determined automatically
#' @import XLConnect
#' @export
read.trapping <- function(dir.in=NULL,
                          nfile="Woodland Reserve Fauna Data.xlsx",
                          sheet="DB",
                          last.row=0) {
  if(last.row == 0) {
        message("Warning: last.row was left to default value.
                Please check that the last rows contain correct data")
  }

  if(is.null(nfile)) {
    message("Please, select the file to import")
    full.path <- file.choose()
    nfile <- basename(full.path)
    dir.in <- dirname(full.path)
  } else {
    if(is.null(dir.in))
      dir.in <- choose.dir(default= path.expand('~'),
                           "Select the folder where the trapping data file is")
  }

  message(paste("Reading data file", nfile, "..."))
  data <- readWorksheetFromFile(
    paste0(dir.in, "/", nfile),
    sheet=sheet, endRow=last.row)
  message("Headings of imported data:")
  print(names(data))

  message("Conducting basic quality checks...")

  message(paste("Found", length(unique(data$Species)), "species"))
  print(unique(data$Species))

  message(paste("Found", length(unique(data$Sex)), "codes for sex"))
  print(unique(data$Sex))

  message(paste("Found", length(unique(data$Location)), "codes for Location"))
  print(unique(data$Location))

  message(paste("Found", length(unique(data$Age)), "codes for Age"))
  print(unique(data$Age))

  if(!is.numeric(data$Animal.Weight)){
    message("Warnings: Animal.wights is not a numeric vector")
  } else {
    message("Animal.Weight: OK")
  }
  if(!is.numeric(data$Pes)) {
    message("Warnings: Pes is not a numeric vector")
  } else {
    message("Animal.Weight: OK")
  }
  if(!is.numeric(data$Crown)) {
    message("Warnings: Crown is not a numeric vector")
  } else {
    message("Crown: OK")
  }
  if(!is.numeric(data$GW)) {
    message("Warnings: GW is not a numeric vector")
  } else {
    message("GW: OK")
  }

  message(paste("Found", length(unique(data$PY)), "codes for PY"))
  print(unique(data$PY))

  if(!is.numeric(data$PY.CR)) {
    message("Warnings: PY.CR is not a numeric vector")
  } else {
    message("PY.CR: OK")
  }

  if(!is.numeric(data$Tick.Count) | !is.integer(data$Tick.Count)) {
    message("Warnings: Tick.Count is not a numeric or integer vector")
  } else {
    message("Tick.Count: OK")
  }
  return(data)
}

