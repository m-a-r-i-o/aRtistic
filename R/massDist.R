#' massDist function
#'
#' This function plots the mass distribution of heaviest black holes
#' @keywords mass distribution
#' @export
#' @examples
#' massDist()

massDist <- function()
{
    library("ggplot2")
    library("ggthemes")
    library("Cairo")

    holes <- read.table("holes", header = FALSE)
    masses <- holes$V2

    CairoPDF("massdist.pdf")

    ggplot(data=holes, aes(log10(masses))) + geom_histogram(binwidth=0.2, fill = "#4682B4") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(axis.line = element_line(colour = "#707070")) + labs(x=expression(log[10](M/M["Sun"])), y="counts")
}

