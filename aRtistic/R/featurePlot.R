feature_plot <- function(rbin, ds)
{
    pdf("featureplot.pdf")
    plot(rbin, ds, pch = 16, xlab = expression(R/R[h]), ylab = expression(S))
    le <- length(rbin) - 1
    labels <- paste(c(rep("S", le+1), rep("R", le+1)), c(0:le, 0:le), sep = "")
    par(fig=c(0.3,0.99,0.3,0.99), new = TRUE)
    groups <- as.factor(c(rep("S", le+1), rep("R", le+1)))
    dotchart(c(ds/ds[round(0.5*length(ds))],rbin), labels = labels, groups = groups, )
    dev.off()
}

