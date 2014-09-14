pollutantmean <- function(directory, pollutant, id = 1:332) {
  #location of files
  path = directory
  
  #list all the file names in that location
  files = list.files(path)
  filenames = as.numeric(sub("\\.csv$","",files))
  
  #build data frame from selected files 
  selectedfiles = files[match(id,filenames)]
  Data = lapply(file.path(path,selectedfiles),read.csv)
  Data = do.call(rbind.data.frame,Data)
  
  #calculate mean
  mean(Data[,pollutant],na.rm=TRUE)
  
}