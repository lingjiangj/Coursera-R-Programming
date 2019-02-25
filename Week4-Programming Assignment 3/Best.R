best <- function(state,outcome){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv")
    
    ## Check that state and outcome are valid
    states <- data[,"State"]
    valid_state <- FALSE
    for (i in 1:length(states)){
        if (state == states[i]){
            valid_state <- TRUE
            break
        }
    }
    if (valid_state == FALSE){
        stop ("invalid state")
    }
    if (!((outcome == "heart attack")|(outcome == "heart failure")|(outcome == "pneumonia"))){
        stop ("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
    col <- if(outcome == "heart attack"){
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    }else if(outcome == "heart failure"){
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    }else {
        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
    data[, "Hospital.Name"] <- as.character(data[, "Hospital.Name"])
    statedata <- data[grep(state, data$State), ]
    orderdata <- statedata[order(statedata[, col], statedata[, "Hospital.Name"], na.last = NA), ]
    orderdata[1, 2]
}


# Here is some sample output from the function
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")
