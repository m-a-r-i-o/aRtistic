#' impute function
#'
#' This function imputes NAs and Infs with column median
#' @param df a dataframe
#' @keywords impute, NA, Inf
#' @export
#' @examples
#' a <- sample(c(NA, 3, 7), 100, replace = TRUE)
#' b <- runif(100)
#' impute(data.frame(a,b))
impute <- function(df)
{
    for(i in 1:ncol(df))
    {
      df[is.na(df[,i]) | is.infinite(df[,i]), i] <- median(df[,i], na.rm = TRUE)
    }
    warning("NA's in features, imputing...")
    return(df)
}

