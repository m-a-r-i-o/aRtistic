plotSnap <- function()
{
    snap.pro <- read.table("snap.pro", header = T)
    summary(snap.pro)
    li <- 40*c(-1, 1)
    jpeg("xy.jpg")
    plot(snap.pro$x, snap.pro$y, xlim = li, ylim = li, pch = ".", xlab = "x", ylab = "y")
    jpeg("xz.jpg")
    plot(snap.pro$x, snap.pro$z, xlim = li, ylim = li, pch = ".", xlab = "x", ylab = "z")
    jpeg("yz.jpg")
    plot(snap.pro$z, snap.pro$y, xlim = li, ylim = li, pch = ".", xlab = "z", ylab = "y")
}
