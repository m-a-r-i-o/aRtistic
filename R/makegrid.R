#' makegrid function
#'
#' This function takes a numeric dataframe and outputs a random grid of
#' points covering the volume spanned by the dataframe in column space
#' the number of rows is k that of the original dataframe
#' @param df numeric dataframe
#' @param k fill factor
#' @param epsi generate random numbers in the range [(1-epsi)*min, (1+epsi)*max[
#' @keywords grid
#' @export
#' @examples
#' makegrid(data.frame(u=rnorm(10),v=rnorm(10)))

makegrid <- function(df, k=1, epsi = 0.1)
{
    grid <- apply(df, 2, function(x) {
    	                      mi <- (1-epsi)*min(x)
    	                      ma <- (1+epsi)*max(x)
    	                      l <- k*length(x)
                              return(mi+runif(l)*(ma-mi))
                              })
    return(grid)
}