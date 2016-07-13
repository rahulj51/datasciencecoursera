pollutantmean <- function(directory, pollutant, id=1:332) {
    #initialize 
    p <- numeric()
    
    for (i in id) {
        #read id file
        table <- readFile(directory, i)
        pollutantValues <- table[,pollutant]
        
        #aggregate values
        start <- length(p) + 1
        end <- start + length(pollutantValues) -1
        p[start:end] <- pollutantValues
    }
    
    mean(p, na.rm = TRUE)
}

readFile <- function(directory, monitor) {
    colNames <- c("Date","sulfate","nitrate","ID")
    colClasses <- c("Date","numeric", "numeric", "integer")
    fileName <- paste0(directory, "/", sprintf("%03d",monitor), ".csv")
    read.csv(fileName, col.names=colNames, colClasses = colClasses)
}
