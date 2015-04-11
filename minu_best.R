setwd("C://Users//risto//Documents//Minu asjad//Statistika, mudelid, excel//R//Coursera//courses-master//02_RProgramming//homework/data")

best <- function(state, outcome) {
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
    andmed3$Hospital.Name=as.character(andmed3$Hospital.Name)
    #sordime
    newdata <- andmed3[order(andmed3[,3], andmed3[,1]),] 
    newdata[1,1]
}