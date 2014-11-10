##Set working directory to data directory
setwd('C:/Users/bryan_000/Documents/GitHub/Data/')

corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        id <- c(1:332)
        z <- numeric(0)
        directory <- paste(getwd(),directory,sep="/")
        for(i in 1:length(id))
                
        {
                if (nchar(id[i])==1) {id[i] <- paste("00",id[i],sep="")}
                else
                if (nchar(id[i])==2){id[i] <- paste("0",id[i],sep="")}
                        id[i] <- paste(id[i],"csv",sep=".")
                        tempfile <- paste(directory,id[i],sep="/")
                        avedata <- read.csv(tempfile)
                                if (sum(complete.cases(avedata)) > threshold)
                                        {
                                         z <- c(z, signif(cor(avedata[,2], avedata[,3], use = "pairwise.complete.obs"), digits =3))
                                         }
        }
        return(z)                       
}