FOVselect <- function(x, y, xi_FOV, alpha_FOV, beta_FOV, rh)
{   #Select stars that are within the field of view
    x_FOV <- xi_FOV*rh
    a_FOV <- alpha_FOV*rh
    b_FOV <- beta_FOV*rh
    withinFOV_x <- abs(x - x_FOV) < a_FOV
    withinFOV_y <- abs(y) < b_FOV
    wFOV <- withinFOV_x & withinFOV_y
    return(wFOV)
}
