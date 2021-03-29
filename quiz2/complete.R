complete <- function(directory, id = 1:332){
  files_list <- list.files(directory, full.names=TRUE)   
  dat <- data.frame()
  for (i in id) {                                
    file <- read.csv(files_list[i])
    file1 <- na.omit(file)
    file2 <- data.frame("id"=file1[1,4], "nobs"=nrow(file1))
    dat <- rbind(dat, file2)
  }
  dat
}