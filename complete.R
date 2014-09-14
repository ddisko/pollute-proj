complete <- function(directory, id) {
  s <- vector()
  for (i in 1:length(id)) {
    path <- c(paste(directory, "/",formatC(id[i], width=3, flag=0),".csv",sep=""))   
    data <- c(read.csv(path)) 
    s[i] <- sum(complete.cases(data))
  } 
  dat <- data.frame(cbind(id,nobs=s))   
  return(dat)
}