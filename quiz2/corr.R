corr <- function(directory, threshold = 0){
  files_list <- list.files(directory, full.names=TRUE)   
  res <- 0
  for (i in 1:332) {                                
    file <- read.csv(files_list[i])
    file1 <- na.omit(file)
    if (nrow(file1) > threshold){
      res <- append(res, cor(file1$sulfate, file1$nitrate))
    }
  }
  res
}