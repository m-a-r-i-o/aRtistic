#' select_visibles function
#'
#' This function selects only visible star types from the snapshot
#' @param snap dataframe obtained from reading an extended snapshot
#' @param mintype minimum star type to include (defaults to -1, don't touch it)
#' @param maxtype maximum star type to include (I chose 10 at first, try 5 or even 3)
#' @keywords read MOCCA snapshot
#' @export
#' @examples
#' select_visibles()

select_visibles <- function(snap, mintype = -1, maxtype = 10)
{
    startype <- snap$V8
    visiblestars <- (startype > mintype) & (startype < maxtype)
    if(length(startype[visiblestars]) < 0.1*length(startype)) warning("Stars of a visible type are less than 10 percent of the total")
    return(snap[visiblestars,])
}
