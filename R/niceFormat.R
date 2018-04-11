#' niceFormat function
#'
#' This function is used internally
#' @export

niceFormat <- function(u)
{
 us <- strsplit(u, "beta")
 us <- lapply(us, insertDot)  
 return(unlist(us))
}
