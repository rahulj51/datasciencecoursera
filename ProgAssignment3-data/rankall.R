source("rankhospital.R")

rankedHospital <- function(mortality, outcome, rank) {
    sortorder <- order(mortality[,outcome], mortality[,'Hospital.Name'], na.last = NA)
    
    if (is.numeric(rank)) {
        index <- sortorder[rank]
    } else {
        index <- switch(rank, best = sortorder[1], worst = sortorder[length(sortorder)])
    }
    
    mortality[index, c('Hospital.Name','State')]
    
}

rankall <- function(outcome, rank = "best") {
    if (! validOutcome(outcome)) stop("invalid outcome") 
    outcome <- make.names(outcome)  #convert to syntactially correct name (spaces in names not allowed)
    
    # group by state and then 
    mortality <- mortality_data_by_state() # will return mortality data for all states
    
    mortalility_list_by_state <- split(mortality, mortality$State)
    
    #apply the order function on each sub-list
    result <- lapply(mortalility_list_by_state, rankedHospital, outcome, rank)
    
    resultTable <- data.frame(t(sapply(result,c))) # converts this to a dataframe http://stackoverflow.com/a/4227504/24424
    colnames(resultTable) <- c("hospital", "state")
    resultTable
    
}