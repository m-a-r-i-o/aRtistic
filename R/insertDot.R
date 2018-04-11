insertDot <- function(u)
{
  uu <- unlist(strsplit(u[2], split=""))
  if(length(uu) > 1)
  {
    uu <- c(uu[1], uu)
    uu[2] <- "."
  }
  uu <- paste(uu, collapse = "")
  return(as.numeric(uu))
}

