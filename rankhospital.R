setwd("C://Users//risto//Documents//Minu asjad//Statistika, mudelid, excel//R//Coursera//courses-master//02_RProgramming//homework")

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcomedata <- read.csv("./data/outcome-of-care-measures.csv", colClasses = "character")
    outcomedata[, 11] <- as.numeric(outcomedata[, 11])
    outcomedata[, 17] <- as.numeric(outcomedata[, 17])
    outcomedata[, 23] <- as.numeric(outcomedata[, 23])
    ## Check that state and outcome are valid
    if (outcome=="heart attack"){
        outcome.number<-11
    }else if(outcome=="heart failure"){
        outcome.number<-17
    }else if(outcome=="pneumonia"){
        outcome.number<-23
    }else{
        stop("invalid outcome")
    }
    #Create array of states
    tempState <- data.frame(table(outcomedata$State))
    tempState<- sapply(tempState, as.character)
    #Test is state parameter is in table
    if(!(state %in% tempState)){
        stop("invalid state")
    }
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    outcomeTemp<-subset(outcomedata, State == state)
    outcomeTemp<-subset(outcomeTemp, outcomeTemp[,outcome.number] != "Not Available")
    outcomeTemp<-outcomeTemp[order(outcomeTemp[,outcome.number], outcomeTemp[,"Hospital.Name"], na.last = TRUE),]
    #check if best, worst, or an integer and that the num isn't greater than the number of rows
    if (num == "best"){
        return(outcomeTemp[1,"Hospital.Name"])
    }else if (num == "worst"){
        return(outcomeTemp[nrow(outcomeTemp), "Hospital.Name"])
    }else if (num > nrow(outcomeTemp)){
        return(NA)
    }else{
        return(outcomeTemp[num,"Hospital.Name"])
    }
}
# tests
rankhospital("MN", "heart attack", 5000)
rankhospital("MD", "heart attack", "worst")
rankhospital("TX", "heart failure", 4)