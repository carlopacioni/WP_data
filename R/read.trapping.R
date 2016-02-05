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
#' \code{read.trapping} will check whether numeric data are stored in numeric
#' vectors and whether morphometric measurements contain values that are zeros
#' (which are clearly not correct for morphometric measurements). A warning is
#' reported in the first case and zero values are replaced with NA in order
#' exclude them from further analyses.
#'
#' The number of codes found for the cateforigal variables and a print out of
#' the code is also reported.
#'
#' @param dir.in The path to the folder where the data file is located
#' @param nfile The name of the (excel) data file
#' @param sheet The name of the sheet to be imported
#' @param last.row An integer with the number of the last row to be imported. If
#'   zero (default), the last row is determined automatically
#' @import XLConnect
#' @return A data.frame
#' @export
read.trapping <- function(dir.in=NULL,
                          nfile="Woodland Reserve Fauna Data.xlsx",
                          sheet="DB",
                          last.row=0) {
  #----------------------------------------------------------------------------#
  # Helper function
  #----------------------------------------------------------------------------#

  cat.check <- function(cat.col, data) {
    message(paste("Found", length(unique(data[, cat.col])), "codes for", cat.col))
    print(unique(data[, cat.col]))
  }

  #----------------------------------------------------------------------------#
  morpho.cols <- c("Animal.Weight", "Pes", "Crown", "GW", "PY.CR")
  cat.cols <- c("Species", "Sex", "Location", "Age")
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

  lapply(cat.cols, cat.check, data)

  num.cols <-c(morpho.cols, c("PY", "Tick.Count"))
  for (i in seq_along(num.cols)) {
    if(!is.numeric(data[, num.cols[i]])) {
      message(paste("Warnings:", num.cols[i], "is NOT a numeric vector"))
    } else {
      message(paste(num.cols[i], "is a numeric vector"))
    }
    if(num.cols[i] %in% morpho.cols) {
      zeros <- data[, num.cols[i]] == 0
      if(sum(zeros, na.rm=TRUE) > 0) {
        message(paste("Found zeros in", num.cols[i], "replaced with NA"))
        data[, num.cols[i]][zeros] <- NA
      }
    }
  }
  return(data)
  }
