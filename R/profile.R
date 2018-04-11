#' profile function
#'
#' This function generates a radial density profile given radii and masses
#' @param r vector with radial positions of stars
#' @param m vector with masses of stars
#' @param profbins number of bins
#' @param feature.plot if TRUE plots some diagnostic plots
#' @keywords profile, radial density profile
#' @export
#' @examples
#' profile(r, m, 8)

profile <- function(r, m, profbins, feature.plot=FALSE)
{
    q <- quantile(r, seq(from = 0.0, to = 1.0, length.out = profbins+1))
    rc <- cut(r, breaks=q,include.lowest=TRUE) #split at quantiles of the radius
    if(anyNA(rc)) stop("Binning generated NAs.")
    rmin <- tapply(r, rc, min)
    rmax <- tapply(r, rc, max)
    rbin <- tapply(r, rc, median) #calculate the median radius of the bins
    As <- pi*(rmax*rmax - rmin*rmin)
    ms <- tapply(m, rc, sum)
    ds <- ms/As #density in the bin
    if(feature.plot)
    {
        feature_plot(rbin, ds)
    }
    if(length(m) < profbins*profbins)
    {
        rbin <- rep(NA, profbins)
        ds <- rep(NA, profbins)
        warning(paste("Less than ", profbins*profbins, " stars in the profile. Returning NAs!"))
    }
    return(c(rbin, ds)) #return as features the quantiles of radius and associated densities
}

