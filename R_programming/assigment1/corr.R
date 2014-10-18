corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  source("complete.R")
  
  df_nobs = complete(directory)
  
  ids = df_nobs[ df_nobs$nobs > threshold, ]$id 

  corrs = vector(mode="numeric")
  
  if(length(ids)> 1){
  
    files = paste(directory, "/", sprintf("%03d", ids), ".csv", sep="")
  
    for (file in files){
      x = read.csv(file)
    
      x_prime = x[complete.cases(x$sulfate) & complete.cases(x$nitrate),]
    
      df = data.frame (sulfate = x_prime$sulfate, nitrate = x_prime$nitrate)
      corrs = append(corrs, cor(df)[1,2], length(corrs))
    }
  }
  
  corrs
}