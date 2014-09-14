corr <- function(directory, threshold = 0) {
  
  path = directory
  
  #store data frame that holds sulfate amount and nitrate amount that meet threshold and are complete cases
  data <- data.frame(sulfate = numeric(0), nitrate = numeric(0))
  
  #get file names
  myfiles <- list.files(path, all.files = TRUE)
  
  #loop through files
  for(i in 1:332) {
    
    #read each file
    current_dataset <- read.csv(myfiles[i])
    
    #check if there are enough compelte cases to meet threshold
    if(sum(complete.cases(current_dataset)) > threshold) {
      
      #get complete cases
      complete_cases <- current_dataset[complete.cases(current_dataset), ]
      
      #add sulfate and nitrate info to table
      data <- rbind(data, data.frame(sulfate = complete_cases$sulfate[i], nitrate = complete_cases$nitrate)[i])
    }
  }
  #get correlation
  cor(data)
}
