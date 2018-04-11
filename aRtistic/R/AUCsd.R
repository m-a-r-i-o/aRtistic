#' AUC function
#'
#' This function calculates 1-sigma error on the AUC
#' for a Receiver Operator Characteristic (ROC) object
#' @param ROC ROC object
#' @keywords AUC, confidence area, 1-sigma
#' @export
#' @examples
#' AUCsd(ROC)

AUCsd <- function(ROC)
{
    if(is.na(ROC))
    {
        return(NA)
    }else{
        fp <- approxfun(ROC$FPR, ROC$TPRp, rule = 2)
        Ip <- integrate(fp, lower = 0, upper = 1)$value
        fm <- approxfun(ROC$FPR, ROC$TPRm, rule = 2)
        Im <- integrate(fm, lower = 0, upper = 1)$value
        return(Ip - Im)
    }
}
