best <- function(state, outcome) {
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
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    sdf = subset(df, df$State == state)
    if (outcome == "heart attack") {
        # "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        ha <- sdf[!is.na(as.numeric(sdf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
        t <- ha[ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(as.numeric(ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
        sort(t$Hospital.Name)
    } else if (outcome == "heart failure") {
        # "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        ha <- sdf[!is.na(as.numeric(sdf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
        t <- ha[ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min(as.numeric(ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
        sort(t$Hospital.Name)
    } else if (outcome == "pneumonia") {
        # "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        ha <- sdf[!is.na(as.numeric(sdf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
        t <- ha[ha$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min(as.numeric(ha$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
        sort(t$Hospital.Name)
    }
}