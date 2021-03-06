#' read_ext_snap function
#'
#' This function reads a MOCCA extended snapshot
#' @param path path where the snapshot is supposed to be
#' @keywords read MOCCA snapshot
#' @export
#' @examples
#' read_ext_snap()

read_ext_snap <- function(path="./")
{   #Read MOCCA extended snapshots
    cmd <- paste("ls ", path, "snap12*.dat", sep = "")
    extended_snapshots <- system(cmd, intern = T)
    if(length(extended_snapshots) > 0)
    {
        snapfile <- extended_snapshots[1] #read the 1st extended snapshot found
        #read but do not include the first line (it is a summary, not a header) 
        snap <- read.table(snapfile, header = F, skip = 1)
        return(snap)
    } else {
        warning(paste("No extended snapshot found in ", path, sep = ""))
        return(NA)
    }
}

