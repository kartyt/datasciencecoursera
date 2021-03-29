pollutantmean <- function(directory, pollutant, id = 1:332){
      files_list <- list.files(directory, full.names=TRUE)   
      dat <- data.frame()                           
      for (i in id) {                                
            dat <- rbind(dat, read.csv(files_list[i]))
      }
      if (pollutant == "sulfate"){
            dat_subset <- na.omit(dat$sulfate)
            as.numeric(dat_subset)
      }
      if (pollutant == "nitrate"){
            dat_subset <- na.omit(dat$nitrate)
            as.numeric(dat_subset)
      }
      mean(dat_subset)
}