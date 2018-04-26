#' UniqueIdentifierFromPath function
#'
#' accepts a path to a simulation in the form
#' /data5MM/pasquato/n=700000/zini=0.005/fracb=0.1/w0=3.0/iq=1/isemi=0/ikroupa=0/rbar=120.0/rplum=25.0/kfallb=0 or in the form /data5MM/pasquato/n=700000/zini=0.005/fracb=0.1/w0=3.0/iq=1/isemi=0/ikroupa=0/rbar=120.0/rplum=25.0/kfallb=0/extsnap_1.20077167E+04.dat
#' the latter includes the snapshot file name
#' both are reduced to the first form, which is returned
#' @param DirtyPath path to clean up
#' @keywords housekeeping
#' @export
#' @examples
#' UniqueIdentifierFromPath("/data5MM/pasquato/n=700000/zini=0.005/fracb=0.1/w0=3.0/iq=1/isemi=0/ikroupa=0/rbar=120.0/rplum=25.0/kfallb=0 or in the form /data5MM/pasquato/n=700000/zini=0.005/fracb=0.1/w0=3.0/iq=1/isemi=0/ikroupa=0/rbar=120.0/rplum=25.0/kfallb=0/extsnap_1.20077167E+04.dat")

UniqueIdentifierFromPath <- function(DirtyPath)
{
 SplittedPath <- strsplit(DirtyPath, "/")[[1]]
 l <- length(SplittedPath)
 if(grepl("extsnap", SplittedPath[l]))
 {
    SplittedPath <- SplittedPath[1:(l-1)]
 }
#find the folder named zini=something in the path
#include in the path only the folder above and all folders within
 ziniFolder <- grepl("zini", SplittedPath)
 ziniIndex <- (1:length(SplittedPath))[ziniFolder]
 SplittedPath <- SplittedPath[(ziniIndex-1):length(SplittedPath)]
 CleanPath <- paste(SplittedPath, collapse = "/")
 return(CleanPath)
}
