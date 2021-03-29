## rankhospital() takes three arguments: the 2-character abbreviated name
## of a state (state), an outcome (outcome), and the ranking of a hospital in
## that state for that outcome (num).
## The function reads the outcome-of-care-measures.csv file and returns
## a character vector with the name of the hospital that has the ranking
## specified by the num argument. For example, the call
## rankhospital("MD", "heart failure", 5) would return a character vector
## containing the name of the hospital with the 5th lowest 30-day death rate
## for heart failure.
## The num argument can take values “best”, “worst”, or an integer indicating
## the ranking (smaller numbers are better). If the number given by num is
## larger than the number of hospitals in that state, then the function should
## return NA. Hospitals that do not have data on a particular outcome should
## be excluded from the set of hospitals when deciding the rankings.
## Handling ties. It may occur that multiple hospitals have the same 30-day
## mortality rate for a given cause of death. In those cases ties should be
## broken by using the hospital name.


rankhospital <- function(state, outcome, num="best") {
        
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
        
        ## Return hospital name in that state with best, worst or other 30-day death rate
        if (num == "best"){
                x_sort <- x[order(x[,3], x[,2]), ]
                return(x_sort[1,2])
        }
        else if (num == "worst"){
                x_sort <- x[order(-x[,3], x[,2]), ]
                return(x_sort[1,2])
        }
        else {
                x_sort <- x[order(z[,3], x[,2]), ]
                return(x_sort[num,2])
        }
}
