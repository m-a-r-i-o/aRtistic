#This script plots the snapshots in feature space, showing where the snaps
#with a black hole cluster in feature space and where they do not

justplot <- function(dataset)
{
    pdf("informativePlots.pdf")

    #stripchart of each feature conditional on IMBH Y/N
    ncd <- ncol(dataset)
    sapply(1:(ncd-1), function(i) stripchart(dataset[,i] ~ dataset[,ncd], method="stack", pch = 20))

    #boxplot of each feature conditional on IMBH Y/N
    ncd <- ncol(dataset)
    sapply(1:(ncd-1), function(i) boxplot(dataset[,i] ~ dataset[,ncd]))
}
