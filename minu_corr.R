setwd("C://Users//risto//Documents//Minu asjad//Statistika, mudelid, excel//R//Coursera//courses-master//02_RProgramming//homework/specdata")

corr <- function(directory, threshold = 0) {
    directory=getwd()
    files=list.files(directory)
    #kasutame eelmist funktsiooni ära
    algtabel=complete()
    vajalikud=subset(algtabel, nobs>=threshold) 
    j=1
    cor=c()
    id_len <- nrow(vajalikud)
    cor <- rep(0, id_len)
    for (i in vajalikud$id) {
        vahetabel=read.csv(files[i])
        vahetabel=vahetabel[complete.cases(vahetabel),]
        cor[j]=cor(vahetabel$sulfate, vahetabel$nitrate)
        j=j+1
    }
    cor
}