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
	#k to try: at least 2 clusters, maximum 10 or the sqrt of the number of rows
	maximum_number_of_clusters <- min(10, round(sqrt(nrow(dataset))))
	minimum_number_of_clusters <- 2
	if(maximum_number_of_clusters < minimum_number_of_clusters) stop("Too few datapoints for cluster analysis")
	average_silhouette_width <- function(i) return(pam(dataset, i, stand = TRUE)$silinfo$avg.width)
	silwidths <- sapply(minimum_number_of_clusters:maximum_number_of_clusters, average_silhouette_width)
    output <- list(optimalk = minimum_number_of_clusters+which.max(silwidths)-1, minimum_number_of_clusters = minimum_number_of_clusters, maximum_number_of_clusters = maximum_number_of_clusters, silwidths=silwidths)
	return(output)
}