niceFormat <- function(u)
{
 us <- strsplit(u, "beta")
 us <- lapply(us, insertDot)  
 return(unlist(us))
}
