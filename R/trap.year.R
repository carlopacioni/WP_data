#' Trapping across years
#'
#' \code{trap.year} calculates the number of sessions within each year, for each
#' species.
#'
#' @param data The input data (output from \code{read.trapping})
#' @import data.table
#' @return A data.frame with years for column and species as rows
#' @export
trap.year <- function(data) {
  ntsession <- function(yr, V1) length(V1[format(V1, "%Y") == yr])

  dt <- data.table(data)
  dtuni <- dt[, unique(Date), by="Species"]

  yrs<-sort(unique(format(dt[, unique(Date)][!is.na(dtuni)], format="%Y")))

  tryrs <- dtuni[, lapply(yrs, ntsession, V1), by="Species"]
  setnames(tryrs, c("Species", yrs))
  return(tryrs)
}

