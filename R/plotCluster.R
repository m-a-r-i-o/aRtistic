plot_cluster <- function(x, y, rh, complete, plotfile, incomplete_color, complete_color, FOVbordercolor)
{   #Plot the cluster's stars and the FOV; stars lost due to incompleteness are in a different color
    pdf(plotfile)
    plot(x, y, pch = ".", col = incomplete_color, main = "", xlab = "X", ylab = "Y")
    points(x[complete], y[complete], pch = ".", col = complete_color)
    plotFOV(xi_FOV*rh, alpha_FOV*rh, beta_FOV*rh, FOVbordercolor)
    dev.off()
}
