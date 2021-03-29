## rankall() takes two arguments: an outcome name (outcome) and a hospital
## ranking (num). The function reads the outcome-of-care-measures.csv file and
## returns a 2-column data frame containing the hospital in each state that has
## the ranking specified in num. For example the function call rankall("heart
## attack", "best") would return a data frame containing the names of the
## hospitals that are the best in their respective states for 30-day heart
## attack death rates. The function should return a value for every state
## (some may be NA). The first column in the data frame is named hospital,
## which contains the hospital name, and the second column is named state,
## which contains the 2-character abbreviation for the state name. Hospitals
## that do not have data on a particular outcome should be excluded from the
## set of hospitals when deciding the rankings.
## Handling ties. If there is a tie for the ranked hospitals for a given outcome,
## then the hospital names should be sorted in alphabetical order and the first
## hospital in that set should be chosen 


rankall <- function(outcome, num="best") {
        
        data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE, header=TRUE) ## Read outcome data
        names <- c("heart attack" , "heart failure" , "pneumonia") 
        
        ## Check that outcome variable is valid
        if (!(outcome %in% names)){
                stop("Invalid Outcome")
        }
        
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
        
        ## For each state, find the hospital of the given rank
        x_sort <- xdata[order(xdata[,1],xdata[,3],xdata[,2], na.last=NA), ]
        x_spli <- split(x_sort, x_sort[,1])
        
        x <- lapply(x_spli, function(x_spli, num){
                if (num == "best"){
                        return(x_spli[1,2])
                }
                else if (num == "worst"){
                        y=nrow(x_spli)
                        return((x_spli[y,2]))
                }
                else {
                        return(x_spli[num,2])
                }
        },num)
        
        dfx<-data.frame(unlist(x), names(x), row.names=names(x))
        colnames(dfx) = c("hospital", "state")
        return(dfx)
}
