##Set working directory to data directory
setwd('C:/Users/bryan_000/Documents/GitHub/Data/')

pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
       
        directory <- paste(getwd(),directory,sep="/")
        stotal <-length(id)
        scount <- length(id)
                
        for(i in 1:length(id))
                {
                 if (nchar(id[i])==1) {id[i] <- paste("00",id[i],sep="")}
                     else
                        if (nchar(id[i])==2){id[i] <- paste("0",id[i],sep="")}
                 
                 id[i] <- paste(id[i],"csv",sep=".")
                 tempfile <- paste(directory,id[i],sep="/")
                 avedata <- read.csv(tempfile)
                 stotal[i] <-sum (avedata[,pollutant],na.rm=1) #mean(avedata[,pollutant],na.rm=1)
                 scount[i] <- sum(!is.na(avedata[,pollutant]))
                }
        
        answer <-sum(stotal)/sum(scount)
        round (answer, digits=3)
        
}