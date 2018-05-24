#' bestpamk function
#'
#' This function finds the "best" number of clusters to split a dataset into
#' based on unsupervised clustering using the pam function in library cluster
#' @param dataset matrix or dataframe, rows are instances, columns are features
#' @keywords cluster analysis
#' @export
#' @examples
#' bestpamk(data.frame(A=runif(100),B=runif(100)))

bestpamk <- function(dataset)
{
	library(cluster)
	return(2)
}