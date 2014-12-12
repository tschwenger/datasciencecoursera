#Coursera R programming assingment 1
#Part 1 technically Part 1:4
pollutantmean <- function(directory, pollutant, id = 1:332) {
  files_list <-list.files(directory, full.names = TRUE) #creates list of files
  dat <- data.frame()  #creates an empty data frame
  for (i in id) {
    #loops through the files, rbinding them together
    dat <- rbind(dat, read.csv(files_list[i]))
  } #pollutant is set as the character vector and the if else state indicates which column to pull from
  if(pollutant == "sulfate") {
    sulfate <- mean(dat[,2], na.rm = TRUE)
    return(sulfate)
  } else {
    nitrate <- mean(dat[,3], na.rm = TRUE)
    return(nitrate)
  }
}

##Part 2 technically part 5-7
complete <- function(directory,id = 1:332){
  files_full <- list.files("specdata", full.names = TRUE) #creates the list of files which to pull from
  nobs = numeric() #this set nobs as a numuric
  for (i in id) {
    nobs = c(nobs, sum(complete.cases(read.csv(files_full[i])))) #nobs is set to complete cases
  }
  data <- data.frame(id,nobs)
  data
}

#Part 3 technically part 8-10
corr <- function(directory, threshold = 0){
      files_list <- list.files(directory, full.names=TRUE)
      correlation <- numeric(0) #correlation is set as numeric
      dat<- data.frame()
      complete.cases <- complete(directory) #looking only for complete cases in the directory
      for (i in 1:332){
        if (complete.cases$nobs[i] > threshold){
          dat <- na.omit(read.csv(files_list[i])) #this omits all NAs
          correlation <- c(correlation, cor(dat$sulfate, dat$nitrate, use = "pairwise.complete.obs"))
        }
      }
      correlation
}
