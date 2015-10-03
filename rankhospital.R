rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    states <- unique(df$State)
    if (!(state %in% states)) {
        stop("invalid state")
    }
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    if (num != "best" && num != "worst" && !is.numeric(num)) {
        stop("invalid num")
    }
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    # subset the data for the state
    sdf = subset(df, df$State == state)
    if (outcome == "heart attack") {
        # build a data frame excluding NA values
        ha <- sdf[!is.na(as.numeric(sdf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
        # now order the data set
        ordered <- ha[ order(as.numeric(ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),ha$Hospital.Name),]
    } else if (outcome == "heart failure") {
        ha <- sdf[!is.na(as.numeric(sdf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
        ordered <- ha[ order(as.numeric(ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),ha$Hospital.Name),]
    } else if (outcome == "pneumonia") {
        ha <- sdf[!is.na(as.numeric(sdf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
        ordered <- ha[ order(as.numeric(ha$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),ha$Hospital.Name),]
    }
    if (num == 'best') {
        num = 1
    } else if (num == 'worst') {
        num = nrow(ordered)
    }
    # return the appropriate result
    ordered[num,]$Hospital.Name
    
}