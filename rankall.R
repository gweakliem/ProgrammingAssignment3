rankall <- function(outcome, num = "best") {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that num and outcome are valid
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    if (num != "best" && num != "worst" && !is.numeric(num)) {
        stop("invalid num")
    }
    ## For each state, find the hospital of the given rank
    states <- unique(df$State)
    
    if (outcome == "heart attack") {
        # build a data frame excluding NA values
        ha <- df[!is.na(as.numeric(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
        # now order the data set
        ordered <- ha[ order(ha$State,as.numeric(ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),ha$Hospital.Name),]
        colselector <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome == "heart failure") {
        hf <- df[!is.na(as.numeric(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
        ordered <- hf[ order(as.numeric(hf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),hf$Hospital.Name),]
        colselector <- "$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (outcome == "pneumonia") {
        hf <- df[!is.na(as.numeric(df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
        ordered <- hf[ order(as.numeric(hf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),hf$Hospital.Name),]
        colselector <- "$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    }
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    # return the appropriate result
    sdf <- split(ordered,ordered$State)
    if (num == "best") {
        n = 1
    } else if (num == "worst") {
        n = NULL
    } else {
        n = num
    }
    #p <- lapply(sdf,"[",n,c("Hospital.Name", "State"),drop=FALSE)
    p <- lapply(sdf, selector, col = c("Hospital.Name","State"), row=n, worst=(num == "worst"))
    r <- do.call("rbind",p)
    colnames(r) <- c('hospital','state')
    r
}

selector <- function(DF, col, row=NULL, worst=FALSE){
    #if(!is.null(row)q) return(DF[row, col])
    # return NA if the column is unknown.
    if(!missing("col") && !(col %in% names(DF))) return(NA)
    # return NA if we're asking for a row outside the bounds
    if(!is.null(row) && (row > nrow(DF))) return(NA)
    if(worst) {
        tail(DF[,col, drop=F],1)
    } else {
        DF[row, col, drop=FALSE]
    }
}