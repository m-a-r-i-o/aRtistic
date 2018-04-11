truncate_path <- function(t)
{   #truncates a path to remove the final "/"
    return(substr(t, 1, nchar(t)-1))
}
