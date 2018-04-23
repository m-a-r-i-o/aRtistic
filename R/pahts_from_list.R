#' function paths_from_list
#'
#' This function reads paths from a file named 'list' with one
#' path per line. Each path should contain one simulation.
#' @keywords path, list
#' @export
#' @examples
#' paths_from_list
paths_from_list <- function()
{
    if(file.exists("list")) #check if the file named 'list' exists in this directory
    {
        paths <- readLines("list") #if the file exists read the paths
    } else {
        stop("File 'list' does not exist!") #otherwise complain
    }
    return(paths) #return the list of paths
}