#' plotSnap function
#'
#' This function plots a snapshot in the xy, xz, yz planes
#' @param file.pro file name of the snapshot, defaults to snap.pro
#' @param limits plot limits
#' @keywords plot cluster, plot snapshot, plot, snap, snapshot
#' @export
#' @examples
#' plotSnap(file.pro="thissnap") #plots the snapshot in file thissnap (note that the .pro extension is not needed) with default limits

plotSnap <- function(file.pro="snap.pro", limits=40*c(-1, 1))
{
    snap.pro <- read.table(file.pro, header = T)
    li <- limits
    jpeg("xy.jpg")
    plot(snap.pro$x, snap.pro$y, xlim = li, ylim = li, pch = ".", xlab = "x", ylab = "y")
    jpeg("xz.jpg")
    plot(snap.pro$x, snap.pro$z, xlim = li, ylim = li, pch = ".", xlab = "x", ylab = "z")
    jpeg("yz.jpg")
    plot(snap.pro$z, snap.pro$y, xlim = li, ylim = li, pch = ".", xlab = "z", ylab = "y")
}
