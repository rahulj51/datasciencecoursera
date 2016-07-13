add2 <- function(x,y) {
    x+y
}

above10 <- function(x) {
    filter <- x > 10
    x[filter]
}

columnmeans <- function(dataframe, removeNA = TRUE) {
    
    numberOfCols <- ncol(dataframe)
    means <- numeric(numberOfCols)
    
    for (i in 1:numberOfCols) {
        means[i] <- mean(dataframe[,i], na.rm = removeNA)
    }
    
    means
}