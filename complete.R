##Set working directory to data directory
setwd('C:/Users/bryan_000/Documents/GitHub/Data/')

complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        directory <- paste(getwd(),directory,sep="/")
        stotal <- data.frame(nrows=length(id), ncols=2)
        colnames(stotal) <- c("id","nobs") 
        for(i in 1:length(id))
               
                {
                stotal[i,1] <- id[i]
                        if (nchar(id[i])==1) {id[i] <- paste("00",id[i],sep="")}
                                else
                        if (nchar(id[i])==2){id[i] <- paste("0",id[i],sep="")}
                id[i] <- paste(id[i],"csv",sep=".")
                tempfile <- paste(directory,id[i],sep="/")
                avedata <- read.csv(tempfile)
                stotal[i,2] <- sum(complete.cases(avedata))
                }
        
      stotal
    }