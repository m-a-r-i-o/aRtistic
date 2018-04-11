# generates a transparent version of the given color (for shaded areas)
lightn <- function(colo)
{
    lcolo <- col2rgb(colo, alpha = F)
    hexlcolo <- rgb(red = lcolo[1,1]/255, green = lcolo[2,1]/255, blue = lcolo[3,1]/255, alpha = 0.2)
    return(hexlcolo)
}

