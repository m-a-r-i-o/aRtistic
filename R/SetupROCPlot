#' SetupROCPlot function
#'
#' This function set up the pdf file for plotting the ROC, by setting limits
#' and drawing an optional diagonal line corresponding to random classification
#' @param pdfFile File to write the pdf plot on
#' @param dashedidentity add a (dashed) diagonal identity line
#' @keywords ROC, plot
#' @export
#' @examples
#' SetupROCPlot("myplot.pdf", dashedidentity=FALSE) #plot on myplot.pdf without the identity line

SetupROCPlot <- function(pdfFile, dashedidentity=TRUE)
{
    pdf(pdfFile)
    plot(0:1, 0:1, main = "", xlab = "FPR", ylab = "TPR", type = "n")
    if(dashedidentity) lines(0:1, 0:1, lty = 2)
}