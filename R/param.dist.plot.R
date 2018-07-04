#' Plot variable distribution
#'
#' \code{param.dist.plot} generates an histogram of the variables passed with
#' \code{params}. Data are separated by sex and species.
#'
#' If \code{save2disk==TRUE} a PDF and RDA file will be saved to disk in the
#' directory passed with \code{dir.out} (which will be created if doesn't exist).
#'
#' @param data The data.frame with the trapping data
#' @param params The variables to be plotted
#' @param species The species to be included in the plots. If "all", then all
#'   the species found will be plotted
#' @param save2disk Whether to save files to disk (default FALSE)
#' @param dir.out The path where to save the files if save2disk=TRUE
#' @return A list with a plot for each parameter.
#' @import ggplot2
#' @export
param.dist.plot <- function(data, params, species="all", save2disk=FALSE,
                            dir.out=NULL) {
  if(length(species) == 1)
    if(species == "all") species <- unique(data$Species)
  data <- data[data$Species %in% species]
p <- list()
  for (i in 1:length(params)) {
    d <- ggplot(data, aes_string(x=params[i])) +
         geom_histogram(binwidth=.5, colour="black", fill="white") +
         facet_grid(Species~Sex)

    if(save2disk == TRUE) {
      dir.create(dir.out, showWarnings=FALSE, recursive=TRUE)
      ggsave(paste0(dir.out, "/", "plot_", params[i], ".pdf"), d)
      save(d, file=paste0(dir.out, "/", "plot_", params[i], ".rda"))
    }
    p[[i]] <- d
  }
return(p)
}
