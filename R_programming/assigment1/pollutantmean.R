pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
  
        Sum = 0.0
        count = 0.0
        
        files = paste(directory, "/", sprintf("%03d", id), ".csv", sep="")
        
        for (file in files){
          x = read.csv(file)
          
          data = x[[pollutant]][!is.na(x[[pollutant]])]
          
          Sum = Sum + sum(data)
          count = count + length(data)
        }
        
        Sum / count
}
