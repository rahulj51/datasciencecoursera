source("best.R")

rankhospital <- function(state, outcome, rank = "best") {
    if (! validOutcome(outcome)) stop("invalid outcome") 
    
    outcome <- make.names(outcome)  #convert to syntactially correct name (spaces in names not allowed)
    
    mortality <- mortality_data_by_state(state)
    
    if (nrow(mortality) == 0) stop("invalid state")
    
    # order the mortality data by outcome values (remove NAs) and then by names. pick the rank'th element
    sortorder <- order(mortality[,outcome], mortality[,'Hospital.Name'], na.last = NA)
    

    if (is.numeric(rank)) {
        index <- sortorder[rank]
    } else {
        index <- switch(rank, best = sortorder[1], worst = sortorder[length(sortorder)])
    }
    
    mortality[index, 'Hospital.Name']
}