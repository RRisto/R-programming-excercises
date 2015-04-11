setwd("C://Users//risto//Documents//Minu asjad//Statistika, mudelid, excel//R//Coursera//courses-master//02_RProgramming//homework/specdata")

complete <- function(directory, id = 1:332) {
    directory=getwd()
    files=list.files(directory)
    kokku=c()
    j=1
    for (i in id) {
        vahe=sum(complete.cases(read.csv(files[i]))=="TRUE")
        nobs[j]=vahe
        j=j+1
    }
    kokku=data.frame(id, nobs)
    kokku
}
