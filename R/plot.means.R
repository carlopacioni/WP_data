#' Plot variable means across time
#'
#' \code{plot.means} plots the means of the variables passed with \code{params}.
#' 1.96 x SE bars are also plotted.
#'
#' If \code{save2disk==TRUE} a PDF and RDA file will be saved to disk in the
#' directory passed with \code{dir.out}.
#' @param data The data.frame with the trapping data
#' @param params The variables to be plotted
#' @param species The species to be included in the plots. If "all", then all
#'   the species found will be plotted
#' @param save2disk Whether to save files to disk (default FALSE)
#' @param dir.out The path where to save the files if save2disk=TRUE
#' @import data.table
#' @import zoo
#' @export
plot.means <- function(data, params, species="all", save2disk=FALSE, dir.out=NULL) {
  dt <- data.table(data)
  if(species == "all") species <- dt[, unique(Species)]
  setkey(dt, Species)
  monyr<-dt[,  format(Date, format="%b %Y")]
  lev<-unique(monyr)
  rem <- is.na(lev)
  lev<-lev[!rem]
  lev<- as.yearmon(lev)
  lev<-sort(lev)
  monyr<-as.factor(monyr)
  levels(monyr)<-lev
  dt[, Time := monyr]
  se <- function(x, ...) sd(x, ...) / sqrt(length(x))
  dtmeans <- dt[J(species), lapply(.SD, mean, na.rm = TRUE), .SDcols=params,
                by=.(Species, Time)]
  dtses <- dt[J(species), lapply(.SD, se, na.rm = TRUE), .SDcols=params,
              by=c("Species", "Time")]
plot <- list()
  for (i in 1:length(params)) {
    lower <- dtmeans[, params[i], with=FALSE] -
      1.96 * dtses[, params[i], with=FALSE]
    setnames(lower, "lower")
    upper <- dtmeans[, params[i], with=FALSE] +
      1.96 * dtses[, params[i], with=FALSE]
    setnames(upper, "upper")
    dtmeans[, c("Upper", "Lower") := list(upper[,upper], lower[,lower])]

    d <- ggplot(dtmeans,
                aes_string(x="Time", y=params[i])) +
      geom_point() +
      facet_grid(Species~.) +
      geom_errorbar(aes(ymax=Upper, ymin=Lower), width=0.15) +
      theme(axis.text.x=element_text(angle=-45, vjust=1))
    if(save2disk == TRUE) {
      ggsave(paste0(dir.out, "/", "plot_", params[i], ".pdf"), d)
      save(d, file=paste0(dir.out, "/", "plot_", params[i], ".rda"))
    }
    plot[[i]] <- d
  }
return(plot)
}
