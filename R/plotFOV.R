#' plotFOV function
#'
#' This function plots the FOV as lines on top of a plot
#' @param xi_FOV x position of the FOV in original MOCCA units (i.e. not scaled to rh)
#' @param a_FOV x size of the FOV in original MOCCA units (i.e. not scaled to rh)
#' @param b_FOV y size of the FOV in original MOCCA units (i.e. not scaled to rh)
#' @param col color to plot the FOV border
#' @keywords plot cluster
#' @export
#' @examples
#' plotFOV(0, 1, 1, "seagreen") #plots a square, centered FOV in seagreen

plotFOV <- function(x_FOV, a_FOV, b_FOV, col)
{   #Plot the (rectangular) field of view on top of the image
    lines(c(x_FOV - a_FOV, x_FOV + a_FOV), c(-b_FOV, -b_FOV), col = col)
    lines(c(x_FOV - a_FOV, x_FOV + a_FOV), c(b_FOV, b_FOV), col = col)
    lines(c(x_FOV - a_FOV, x_FOV - a_FOV), c(-b_FOV, b_FOV), col = col)
    lines(c(x_FOV + a_FOV, x_FOV + a_FOV), c(-b_FOV, b_FOV), col = col)
}
