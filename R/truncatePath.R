#' truncate_path function
#'
#' This function truncates the last character off a path (to remove the final "/")
#' @param t path to truncate
#' @keywords path
#' @export
#' @examples
#' truncate_path("Ciao bello!") #should return "Ciao bello" (without the "!")

truncate_path <- function(t)
{
    return(substr(t, 1, nchar(t)-1))
}
