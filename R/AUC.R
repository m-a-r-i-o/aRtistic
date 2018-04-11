#' AUC function
#'
#' This function calculates the area under the curve (AUC)
#' for a Receiver Operator Characteristic (ROC) object
#' @param ROC ROC object
#' @keywords AUC
#' @export
#' @examples
#' AUC(ROC)

AUC <- function(ROC)
{
    if(is.na(ROC))
    { 
        return(NA)
    }else{
        f <- approxfun(ROC$FPR, ROC$TPR, rule = 2)
        I <- integrate(f, lower = 0, upper = 1)$value
        return(I)
    }
}

