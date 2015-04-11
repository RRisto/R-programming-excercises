setwd("C://Users//risto//Documents//Minu asjad//Statistika, mudelid, excel//R//Coursera//courses-master//02_RProgramming//homework/data")

rankhospital <- function(state, outcome, num = "best") {
    andmed=read.csv("outcome-of-care-measures.csv")
    library(plyr)
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop("invalid outcome")
    }
    if (!state %in% andmed$State) {
        stop("invalid state")
    }
    andmed2=rename(andmed, c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"="heart attack",
                             "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"="heart failure",
                             "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"="pneumonia"))
    variables=c("Hospital.Name", "State", outcome)
    #jätamr alles ainult variabled mida vaja
    andmed2=andmed2[variables]
    #valime vajaliku maakonna
    andmed2=subset(andmed2, State==state)
    #eemaldame puuduvad andmed
    andmed3=andmed2[ which(andmed2[,3]!="Not Available"),]
    #teeme numbrid numericuks
    andmed3[,3]=as.numeric(as.character(andmed3[,3]))
    #sordime
    newdata <- andmed3[order(andmed3[,3], andmed3[,1]),]
    newdata$Hospital.Name=as.character(newdata$Hospital.Name)
    if (num=="best") {
        vastus=newdata[1,1]
    }
    else if (num=="worst") {
        vastus=tail(newdata) [6,1]
    }
    else if (num <= nrow(newdata)) {
        vastus=newdata[num, 1]
    }
    else if (num > nrow(newdata)) {
       vastus= NA
    }
    vastus
}