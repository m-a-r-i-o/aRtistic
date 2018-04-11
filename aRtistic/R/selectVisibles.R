select_visibles <- function(snap, mintype = -1, maxtype = 10)
{   #Select only visible star types from the snapshot
    startype <- snap$V8
    visiblestars <- (startype > mintype) & (startype < maxtype)
    if(length(startype[visiblestars]) < 0.1*length(startype)) warning("Stars of a visible type are less than 10 percent of the total")
    return(snap[visiblestars,])
}
