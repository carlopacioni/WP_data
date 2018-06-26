#' A function to compute the number of animal known to be alive and plot it over
#' time for each species
#'
#' @param animal.ids the header of the column with the animals' IDs
#' @inheritParams dot.plot
#' @import data.table
#' @import ggplot2
#' @export
#' @return A list wher the first element is a plot and the second are the data
KTBA <- function(data, species="all", animal.ids="M.chip.serial..") {

  if(length(species) == 1) {
    if(species == "all") species <- unique(data[, "Species"])
  }

    sdat <- data[data[, "Species"] %in% species,]
    t <- table(sdat[, "Species"], sdat[, animal.ids], sdat[, "Date"])
    det <- data.table(as.data.frame(t))
    setnames(det, c("Var1", "Var2", "Var3"), c("Species", "Animal.ids", "Date"))
    det[, Date := as.Date(Date)]
    minAndMax <- det[Freq>0,]
    minAndMax[, ':='(min.Date=min(Date, na.rm=TRUE), max.Date=max(Date, na.rm=TRUE)),
      by=c("Species", "Animal.ids")]
    setkey(minAndMax, "Animal.ids")
    minAndMax <- minAndMax[.(unique(Animal.ids)), .SD, mult="first"]
    minAndMax[, ':='(Freq=NULL, Date=NULL)]
    setkeyv(minAndMax, c("Species", "Animal.ids"))
    setkeyv(det, c("Species", "Animal.ids"))
    det <- merge(det, minAndMax, all.x = TRUE)
    det[, Alive := ifelse(Date>=min.Date & Date<=max.Date, 1, 0)]
    det.fin <- det[, .(KTBA=sum(Alive, na.rm = TRUE)), by=c("Species", "Date")]

    p <- ggplot(det.fin, aes(Date, KTBA)) + geom_line() + facet_grid(Species~.)
  return(list(Plot=p, Data=det.fin))
}
