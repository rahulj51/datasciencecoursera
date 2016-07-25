options(warn=-1)

possible_outcomes <- c("heart attack", "heart failure", "pneumonia")

mortality_data_by_state <- function(state = NULL) {
    all_outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    mortality <- all_outcomes[,c(2,7,11,17,23)] #extract only the columns we are interested in
    
    mortality[,3:5] <- apply(mortality[,3:5], 2, function(x) as.numeric(as.character(x))) #convert to numeric
    
    #rename columns
    colnames(mortality)[3:5] <- possible_outcomes 
    colnames(mortality) <- make.names(names(mortality))
    
    if (! is.null(state)) {
        mortality <- subset(mortality, State == state)
    }
    
    mortality
}

validOutcome <- function(outcome) {
    ! is.na(match(outcome, possible_outcomes))
}

best <- function(state, outcome) {
    
    # error out if outcome is invalid
    if (! validOutcome(outcome)) stop("invalid outcome") 
    
    outcome <- make.names(outcome)  #convert to syntactially correct name (spaces in names not allowed)
    
    mortality <- mortality_data_by_state(state)
    
    if (nrow(mortality) == 0) stop("invalid state")
    
    # search for min
    min <- min(mortality[,outcome], na.rm=TRUE)

    # do null check here for min value. possible if all are NA.
    
    best_hospitals <- mortality[mortality[,outcome] == min & !is.na(mortality[,outcome]),]
    
    best_row <- 1
    
    if (nrow(best_hospitals) > 1) {
        print("resolving tie")
        # there is a tie. Multiple rows with min value of outcome
        # sort by hospital names and pick the first by name
        best_row <- order(best_hospitals$Hospital.Name)[[1]]
    }
    
    best_hospitals[best_row, 'Hospital.Name']
}