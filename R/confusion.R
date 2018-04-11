#' confusion function
#'
#' This function returns a list with the false positive rate (FPR)
#' and true positive rates (TPR) for a ROC object. The TPR and FPR
#' are calculated at the furthest point of the ROC curve from the
#' diagonal identity line, i.e. at the optimal threshold for the
#' classifier assuming equal cost for false positives and false negatives 
#' @param ROC ROC object
#' @keywords FPR, TPR, optimal point
#' @export
#' @examples
#' confusion(ROC)

confusion <- function(ROC)
{
    if(is.na(ROC))
    {
        return(NA)
    }else{ 
        x <- ROC$FPR
        y <- ROC$TPR
        v <- (y - x)/(2**0.5)
        vi <- which.max(v)
        optimalFPR <- x[vi]
        optimalTPR <- y[vi]
        optimalconfusion <- list(FPR = optimalFPR, TPR = optimalTPR)
        return(optimalconfusion)
    }
}
