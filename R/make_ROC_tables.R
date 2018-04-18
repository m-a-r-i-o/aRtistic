#' make_ROC_tables function
#'
#' This puts ROCs read from ROC files together in a table
#' @param none
#' @keywords ROC
#' @export
#' @examples
#' make_ROC_tables()
make_ROC_tables <- function()
{
	ROCs <- system("ls | grep ROCs_", intern = T)
    ROC_tables <- read.table(ROCs[1], header = T)
    for (i in 2:length(ROCs))
    {
        ROC_tables <- cbind(ROC_tables, read.table(ROCs[i], header = T))
    }
    return(ROC_tables)
}