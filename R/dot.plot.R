#' Plot variable means across time
#'
#' \code{dot.plot} plots the means of the variables passed with \code{params}.
#' 1.96 x SE (confidence intervals) bars are also plotted.
#'
#' If \code{save2disk==TRUE} a PDF and RDA file will be saved to disk in the
#' directory passed with \code{dir.out} (which will be created if doesn't exist).
#' Size of the saved plots can be controlled with \code{wcm} and \code{hcm}.
#' Default settings save plots with a landscape orientation on A4 size.
#'
#' @param data The data.frame with the trapping data
#' @param params The variables to be plotted
#' @param species The species to be included in the plots. If "all", then all
#'   the species found will be plotted
#' @param save2disk Whether to save files to disk (default FALSE)
#' @param dir.out The path where to save the files if save2disk=TRUE
#' @param wcm width of saved plots in cm
#' @param hcm height of saved plots in cm
#' @return A list with a plot for each parameter. The last element is a table
#' with mean and confidence intervals for each parameter
#' @import data.table
#' @import ggplot2
#' @import zoo
#' @export
dot.plot <- function(data, params, species="all", trendline=TRUE,
                     save2disk=FALSE, dir.out=NULL, wcm=27, hcm=19) {
  dt <- data.table(data)
  suppressWarnings(if(species == "all") species <- dt[, unique(Species)])
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
                by=.(Species, Sex, Time)]
  dtses <- dt[J(species), lapply(.SD, se, na.rm = TRUE), .SDcols=params,
              by=.(Species, Sex, Time)]
p <- list()
  for (i in 1:length(params)) {
    lower <- dtmeans[, params[i], with=FALSE] -
      1.96 * dtses[, params[i], with=FALSE]
    setnames(lower, "lower")
    upper <- dtmeans[, params[i], with=FALSE] +
      1.96 * dtses[, params[i], with=FALSE]
    setnames(upper, "upper")
    dtmeans[, c(paste0(params[i], "Upper"), paste0(params[i], "Lower")) :=
              list(upper[,upper], lower[,lower])]

    d <- ggplot(dtmeans, aes_string(x="Time", y=params[i])) +
      geom_point() +
      facet_grid(Species~Sex) +
      geom_errorbar(aes_string(ymax=paste0(params[i], "Upper"),
                               ymin=paste0(params[i], "Lower")), width=0.15) +
      theme(axis.text.x=element_text(angle=-90, vjust=1))
    if(trendline) d <- d + geom_smooth()

    if(save2disk == TRUE) {
      dir.create(dir.out, showWarnings=FALSE, recursive=TRUE)
      ggsave(paste0(dir.out, "/", "plot_", params[i], ".pdf"), d)
      save(d, file=paste0(dir.out, "/", "plot_", params[i], ".rda"))
    }
    p[[i]] <- d
  }
i <- i + 1
p[[i]] <- dtmeans
return(p)
}
