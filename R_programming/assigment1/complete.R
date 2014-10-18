complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  files = paste(directory, "/", sprintf("%03d", id), ".csv", sep="")
  
  nobs = c()
  ids = c()
  
  for(file in files){
    x = read.csv(file)
    nobs = append(nobs, length(x[complete.cases(x$sulfate) & complete.cases(x$nitrate),][,1]), length(nobs))
    ids = append(ids, x$ID[1], length(ids))  
  }

  data.frame(id=ids, nobs=nobs)
}