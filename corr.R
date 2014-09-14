corr <- function(directory, threshold = 0) {
    
  # create vector to hold correlation values
  corrsvalue <- numeric(0)
  
  # get files that have complete sets using complete function
  nobsdframe <- complete("specdata")
  
  # test for threshold as set by input
  nobsdframe <- nobsdframe[nobsdframe$nobs > threshold, ]
  
  for (cid in nobsdframe$id) {
    # assemble data
    targetdframe <- readmaindata(cid, directory)
    
    # run correlation processon targeted data
    corrsvalue <- c(corrsvalue, cor(targetdframe$sulfate, targetdframe$nitrate, use = "pairwise.complete.obs"))
  }
  
    return(corrsvalue)
}

complete <- function(directory, id = 1:332) {
  
  # create vector to hold valid observations - those that have both variables recorded
  validobscount <- numeric(0)
  
  for (cid in id) {
    
    countdframe <- readmaindata(cid, directory)
    
    # count valid observations
    validobscount <- c(validobscount, nrow(na.omit(countdframe)))
  }
  
  # build data frame from valid counts
  data.frame(id = id, nobs = validobscount)
}

readmaindata <- function(id, directory, summarize = FALSE) {
    
  # set directory and set up file name format
  filename <- paste(directory, "/", sprintf("%03d", as.numeric(id)), ".csv", 
                   sep = "")
  
  # read files and create main data frame
  originaldata <- read.csv(filename)
  if (summarize) {
    print(summary(originaldata))
  }
  
  return(originaldata)
}
