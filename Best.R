##Set working directory to data directory
setwd('C:/Users/bryan_000/Documents/GitHub/Data/')

best <- function(state, outcome) {
        ## Read outcome data
        tempdata <- read.csv(file='outcome-of-care-measures.csv', colClasses = 'character')
                if(!any(state == tempdata[,7])) {
                     stop('invalid state')
                }
        ## Check that state and outcome are valid 
        z <- 0
        if(outcome == 'heart attack') {
                z <- 11
        }
        if(outcome == 'heart failure') {
                z <- 17
        }
        if(outcome == 'pneumonia') {
                z <- 23
        }
       
        if (z == 0)
           {
               stop('invalid outcome')
        }
    
        ## Return hospital name in that state with lowest 30-day death rate
        statepulled <- tempdata[tempdata [,7] == state, ]
        statepulled[, z] <- as.numeric(x=statepulled[, z])
        statepulled <- statepulled[complete.cases(statepulled), ]
       
        hospnames <- statepulled[(statepulled[, z] == min(statepulled[, z])), ]$Hospital.Name
        
        sort(hospnames)[1]
        
        
}
