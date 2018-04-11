#' plot_cluster function
#'
#' This function plots a cluster from x,y star positions
#' @param x vector with x positions of stars
#' @param y vector with y positions of stars
#' @param rh scale radius (half mass...)
#' @param complete boolean vector that is true for stars that pass completeness (i.e. are included)
#' @param plotfile name of the PDF file to plot on
#' @param incomplete_color color to plot the excluded stars in (due to completeness)
#' @param complete_color color to plot the included stars in 
#' @param FOVbordercolor color to plot the FOV border
#' @param xi_FOV x position of the FOV in units of rh
#' @param alpha_FOV x size of the FOV in units of rh
#' @param beta_FOV y size of the FOV in units of rh
#' @keywords plot cluster
#' @export
#' @examples
#' #plot a cluster with 1000 stars normally distributed in x,y with completeness 0.8
#' plot_cluster(rnorm(1000),rnorm(1000),1,runif(1000) < 0.8, "cluster.pdf", "#888888", "#000000", "red", 0, 1, 1)

plot_cluster <- function(x, y, rh=1, complete, plotfile, incomplete_color="#888888", complete_color="#000000", FOVbordercolor="#FF0000", xi_FOV=0, alpha_FOV=1, beta_FOV=1)
{   #Plot the cluster's stars and the FOV; stars lost due to incompleteness are in a different color
    pdf(plotfile)
    plot(x, y, pch = ".", col = incomplete_color, main = "", xlab = "X", ylab = "Y")
    points(x[complete], y[complete], pch = ".", col = complete_color)
    plotFOV(xi_FOV*rh, alpha_FOV*rh, beta_FOV*rh, FOVbordercolor)
    dev.off()
}
