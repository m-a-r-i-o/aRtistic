#' makegrid function
#'
#' This function takes a numeric dataframe and outputs a random grid of
#' points covering the volume spanned by the dataframe in column space
#' the number of rows is the same as that of the original dataframe
#' @param df numeric dataframe
#' @keywords grid
#' @export
#' @examples
#' makegrid(data.frame(u=rnorm(10),v=rnorm(10)))

makegrid <- function(df)
{
    grid <- apply(df, 2, function(x) {
    	                      mi <- min(x)
    	                      ma <- max(x)
    	                      l <- length(x)
                              return(mi+runif(l)*(ma-mi))
                              })
    return(grid)
}