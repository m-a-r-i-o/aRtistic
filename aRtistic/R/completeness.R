#' AUC function
#'
#' This function returns the probability that a star at rho and L is seen
#' by using an analytical recipe for completeness
#' @param R2D vector of 2D- radial positions of stars
#' @param R2Dcut scale radius (e.g. half-mass radius)
#' @param L vector of stellar luminosities
#' @param Lcut scale luminosity (e.g. 0.5 solar)
#' @keywords completeness
#' @export
#' @examples
#' completeness(r, 1.0, L, 0.5)

completeness <- function(R2D, R2Dcut, L, Lcut) 
{
    co <- return(pmin(1, (L/Lcut)*(1-(1/(1+(R2D/R2Dcut))))))
    return(co)
}

