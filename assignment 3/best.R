## best() function takes two arguments: the 2-character abbreviated name of 
## a state and an outcome name. The function reads the outcome-of-care-measures.csv
## file and returns a character vector with the name of the hospital that has
## the best (i.e. lowest) 30-day mortality for the specified outcome in that state.
## The hospital name is the name provided in the Hospital.Name variable.
## The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”.
## Hospitals that do not have data on a particular outcome should be excluded
## from the set of hospitals when deciding the rankings.
## Handling ties. If there is a tie for the best hospital for a given outcome,
## then the hospital names should be sorted in alphabetical order and the first
## hospital in that set should be chosen 


best <- function(state, outcome) {
       
        data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE, header=TRUE) ## Read outcome data
        names <- c("heart attack" , "heart failure" , "pneumonia") 
       
         ## Check that state and outcome are valid
        if (!(state %in% unique(data[, 7]))){
                stop("Invalid State")
        }
        if (!(outcome %in% names)){
                stop("Invalid Outcome")
        }
      
        
        ## Building data frame
        if (outcome == "heart attack"){
                outcol = data[, 11]
        }
        if (outcome == "heart failure"){
                outcol = data[, 17]
        }
        if (outcome == "pneumonia"){
                outcol = data[, 23]
        }
        xdata <- data.frame(data[, 7], data[, 2], as.numeric(outcol))
        x <- subset(xdata, data[, 7] == state)
        
        ## Return hospital name in that state with lowest 30-day death rate
        x_sort <- x[order(x[,3], x[,2]), ]
        return(x_sort[1,2])
        
}
