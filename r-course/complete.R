completeCases <- function(directory, x) {
    sum(complete.cases(readFile(directory, x)))
}

complete <- function(directory, id = 1:332) {
    
    colNames <- c("id", "nobs")
    
    completeCasesMatrix <- sapply(id, function(x) c(x,completeCases(directory, x)))
    completeCasesFrame <- as.data.frame(t(completeCasesMatrix)) #transpose and coerce as data frame
    setNames(completeCasesFrame, colNames)
}