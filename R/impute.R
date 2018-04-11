# impute NAs and Infs with column median
impute <- function(df)
{
    for(i in 1:ncol(df))
    {
      df[is.na(df[,i]) | is.infinite(df[,i]), i] <- median(df[,i], na.rm = TRUE)
    }
    warning("NA's in features, imputing...")
    return(df)
}

