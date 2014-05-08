## Christian Peikert

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
  
  alldata<-data.frame()
  for(n in id){
    convertNumber <- sprintf("%03d", n)                       ## convert the number in a three-digit number
    apath <- paste(directory,"/",convertNumber,".csv",sep="") ## sep default is a blank
    data <- read.table(apath,sep=",",header=TRUE)             ## read current file
    filtereddata <- data[complete.cases(data),]               ## filtered all uncomplet rows of the current loaded file
    resultmatrix <-cbind(convertNumber,nrow(filtereddata))    ## because the resultmatrix should contain a column id and nobs a matrix is needed
    alldata <- rbind(alldata,resultmatrix)                    ## here the allread loaded data are extended by the current data
  }
  colnames(alldata)<-c("id","nobs")
  alldata
  return(alldata)
}

# complete("D:/coursera/specdata", 1)
# complete("D:/coursera/specdata", c(2, 4, 8, 10, 12))
# complete("D:/coursera/specdata", 30:25)
# complete("D:/coursera/specdata", 3)
