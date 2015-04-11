setwd("C://Users//risto//Documents//Minu asjad//Statistika, mudelid, excel//R//Coursera//courses-master//02_RProgramming//homework/specdata")

pollutantmean <- function(directory, pollutant, id = 1:332) {   
    directory=getwd()
    files=list.files(directory)
    #data=read.csv(files)
    data=lapply(files, read.csv)
    data=do.call(rbind, data)
    #ainult complete cases
    #data2=na.omit(data) 
    data.sub=subset(data, ID%in%id)
    if (pollutant=="sulfate") {
        keskmine=mean(data.sub$sulfate, na.rm=T)
    }
    if (pollutant=="nitrate") {
        keskmine=mean(data.sub$nitrate, na.rm=T)
    }
    keskmine
}