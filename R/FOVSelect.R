#' FOVselect function
#'
#' This function selects stars that are within the field of view
#' @param x vector with x positions of all stars
#' @param y vector with y positions of all stars
#' @param xi_FOV position of the FOV along the x axis in units of rh (xi_FOV = 0 means centered FOV)
#' @param alpha_FOV size of the FOV along the x axis in units of rh
#' @param beta_FOV size of the FOV along the y axis in units of rh
#' @param rh half-mass radius, used to scale xi_FOV, alpha_FOV, beta_FOV
#' @keywords FOV
#' @export
#' @examples
#' FOVselect(x, y, 0, 1, 1, sqrt(median(x*x + y*y))) #square FOV, centered

FOVselect <- function(x, y, xi_FOV, alpha_FOV, beta_FOV, rh)
{   
    x_FOV <- xi_FOV*rh
    a_FOV <- alpha_FOV*rh
    b_FOV <- beta_FOV*rh
    withinFOV_x <- abs(x - x_FOV) < a_FOV
    withinFOV_y <- abs(y) < b_FOV
    wFOV <- withinFOV_x & withinFOV_y
    return(wFOV)
}
