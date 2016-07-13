corr <- function(directory, threshold=0) {
    # corelation for monitors
    
    # complete cases
    completeCases <- complete(directory)
    
    #filter on threshold
    monitorsOfInterest <- subset(completeCases, nobs > threshold)[,1]
    
    #print(monitorsOfInterest)
    correlationVector <- numeric()
    for (i in monitorsOfInterest) {
        table <- readFile(directory, i)
        #print(table)
        correlationVector <- c(correlationVector, cor(table[,2], table[,3], use="complete.obs"))
    }
    correlationVector
}