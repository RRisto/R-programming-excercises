setwd("C://Users//risto//Documents//Minu asjad//Statistika, mudelid, excel//R//Coursera//courses-master//02_RProgramming//homework/data")

rankall <- function(outcome, num = "best") {
    andmed=read.csv("outcome-of-care-measures.csv")
    
    library(plyr)
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop("invalid outcome")
    }
    states=data.frame(unique(andmed$State))
    names(states)="state"
    andmed2=rename(andmed, c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"="heart attack",
                             "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"="heart failure",
                             "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"="pneumonia"))
    variables=c("Hospital.Name", "State", outcome)
    andmed2=andmed2[variables]
    andmed3=andmed2[ which(andmed2[,3]!="Not Available"),]
    #nimetan kolmanda veeru ümber, siis on lihtsam sellele viidata
    names(andmed3)=c("hospital", "state", "data")
    andmed3[,3]=as.numeric(as.character(andmed3[,3]))
    #sordime
    newdata <- andmed3[order(andmed3[,3], andmed3[,1]),]
    #arvutame järgu juurde iga maakonna lõikes
    newdata=ddply(newdata, .(state), transform, rank = rank(state, ties.method="first"))
    
    if (num=="best") {
         tulem=subset(newdata, rank==1, select=c(hospital, state))  
    }
    else if (num <=max(newdata$rank)) {
        tulem=subset(newdata, rank==num, select=c(hospital, state))
        tulem=merge(tulem,states, by="state", all=T)
        tulem=tulem[, c(2,1)]
    }
    else if (num=="worst") { #sellega on jama, kuna minu puhul peab v-d ja W-d 
        #võrdväärseks, muidu loogika on ok
        newdata=andmed3[order(andmed3$state, andmed3$data, decreasing =T),]
        newdata=ddply(newdata, .(state), transform, rank = rank(state, ties.method="first"))
        tulem=subset(newdata, rank==1, select=c(hospital, state))
        tulem=merge(states,tulem, by="state", all=T)
        tulem=tulem[, c(2,1)]
    }
    else if (num >max(newdata$rank)) {
        tulem= NA
    }
 tulem   
}