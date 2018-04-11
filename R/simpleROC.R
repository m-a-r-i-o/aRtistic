#' SimpleROC function
#'
#' This function builds a ROC for a classifier
#' it returns a data frame with TPR and FPR from which to plot the ROC
#' @param labels ground truth boolean vector
#' @param scores numeric vector with probability of a given classification according to the classifier
#' @keywords ROC
#' @export
#' @examples
#' SimpleROC(sample(c(T,F), 1000, replace = T), runif(1000)) #should return an identity line ROC
SimpleROC <- function(labels, scores){
labels <- labels[order(scores, decreasing=TRUE)]
data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels))
}
