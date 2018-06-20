#' Make a Mark input file
#'
#' @param fn File name
#' @param h whether the .csv has headers
#' @param ids whether the first column is animals' ids
#' @param count the count of animals with eac history
make.mark.inp <- function(fn, h=FALSE, ids=TRUE, count=1) {
  make.cap.hist <- function(st, ids=TRUE, count=1) {
    if(ids) {
      id <- as.character(st[1])
      caps <- st[-1]
    } else {
      caps <- st
    }
    det.hist <- paste(if(ids) paste0("/*", id, "*/"),
                      paste0(caps, collapse=""),
                      paste0(count, ";"))
    return(det.hist)
  }
  ####################################
  cap.hist <- read.csv(fn, header = h)
  title <- sub(".csv", "", fn)
  lns <-  apply(cap.hist, 1, FUN = make.cap.hist, ids=ids, count=count)
  writeLines(c(paste0("/*", title, "*/"), lns), con = paste0(title, ".inp"))
  return(c(paste0("/*", title, "*/"), lns))
}
