# Confusion matrix
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
