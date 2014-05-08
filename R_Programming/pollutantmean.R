## Christian Peikert

pollutantmean <- function(directory, pollutant, id = 1:332) { ## 1:332 is the default value
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  alldata<-data.frame()
  for(n in id){
    convertNumber <- sprintf("%03d", n)                       ## convert the number in a three-digit number
    apath <- paste(directory,"/",convertNumber,".csv",sep="") ## sep default is a blank
    data <- read.table(apath,sep=",",header=TRUE)             ## read current file
    alldata <- rbind(alldata,data)                            ## here the allread loaded data are extended by the current data
  }
  pollutantmean = mean(as.matrix(alldata[pollutant]),na.rm=TRUE)
  pollutantmean
  return(pollutantmean) 
}

# pollutant = pollutantmean("D:/coursera/specdata", "sulfate", 1:10)
# pollutant = pollutantmean("D:/coursera/specdata", "nitrate", 70:72)
# pollutant = pollutantmean("D:/coursera/specdata", "nitrate", 23)