plotFOV <- function(x_FOV, a_FOV, b_FOV, col)
{   #Plot the (rectangular) field of view on top of the image
    lines(c(x_FOV - a_FOV, x_FOV + a_FOV), c(-b_FOV, -b_FOV), col = col)
    lines(c(x_FOV - a_FOV, x_FOV + a_FOV), c(b_FOV, b_FOV), col = col)
    lines(c(x_FOV - a_FOV, x_FOV - a_FOV), c(-b_FOV, b_FOV), col = col)
    lines(c(x_FOV + a_FOV, x_FOV + a_FOV), c(-b_FOV, b_FOV), col = col)
}
