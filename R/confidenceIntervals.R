#' confi function
#'
#' This function plots ROC curves and their confidence intervals
#' @keywords ROC
#' @export

confi <- function(j, colo="black", ncurves=8, ncROC, ROC_tables, shadedsigma=2)
{
    iTPR <- j + ncurves*(0:(floor(ncROC/ncurves)-1))
    iFPR <- 1 + j + ncurves*(0:(floor(ncROC/ncurves)-1))
    FPRs <- ROC_tables[,iFPR]
    TPRs <- ROC_tables[,iTPR]
    meanFPR <- rowMeans(FPRs)
    meanTPR <- rowMeans(TPRs)
    sdTPR <- shadedsigma*apply(TPRs, 1, sd)

    polygon(c(meanFPR, rev(meanFPR)), c(meanTPR - sdTPR, rev(meanTPR + sdTPR)), col = lightn(colo), border = NA)
    lines(meanFPR, meanTPR, col = colo)
    return(data.frame(FPR=meanFPR, TPR=meanTPR, TPRp=meanTPR + sdTPR, TPRm=meanTPR - sdTPR))
}

