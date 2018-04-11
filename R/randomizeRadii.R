#' randomize_radii function
#'
#' This function generates x and y positions at random from given 3D radii
#' @param r vector with radial positions of stars
#' @keywords randomize radii
#' @export
#' @examples
#' randomize_radii(runif(100)) #generates 100 x y values from random 3D r values
randomize_radii <- function(r)
{
    l <- length(r)
    theta <- runif(l)*2*pi
    zi <- 2*runif(l) - 1
    u <- sqrt(1 - zi*zi)
    x <- r*u*cos(theta)
    y <- r*u*sin(theta)
    return(list(x=x, y=y))
}
