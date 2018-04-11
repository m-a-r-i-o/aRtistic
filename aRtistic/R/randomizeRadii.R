randomize_radii <- function(r)
{   #Generate x and y positions at random from given 3D radii
    l <- length(r)
    theta <- runif(l)*2*pi
    zi <- 2*runif(l) - 1
    u <- sqrt(1 - zi*zi)
    x <- r*u*cos(theta)
    y <- r*u*sin(theta)
    return(list(x=x, y=y))
}
