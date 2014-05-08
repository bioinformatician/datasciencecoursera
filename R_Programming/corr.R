## Christian Peikert

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  alldata<-numeric()
  for(n in list.files(directory)){
    apath <- paste(directory,"/",n,sep="")                    ## sep default is a blank
    data <- read.table(apath,sep=",",header=TRUE)             ## read current file
    filtereddata <- data[complete.cases(data),]               ## filtered all uncomplet rows of the current loaded file
    observedcases <- nrow(filtereddata)                       ## here the complete rows are counted
    if(observedcases >threshold){                             ## threshold filter
      corrvalues <- cor(filtereddata["sulfate"],filtereddata["nitrate"],use="complete.obs") ## computing of correlation
      alldata <- c(alldata, corrvalues)
    }
  }
  return(alldata)
}


# cr <- corr("D:/coursera/specdata", 150)
# head(cr)
# summary(cr)
# 
# 
# cr <- corr("D:/coursera/specdata", 400)
# head(cr)
# summary(cr)
# 
# cr <- corr("D:/coursera/specdata", 5000)
# summary(cr)
# length(cr)
# 
# cr <- corr("D:/coursera/specdata")
# summary(cr)
# length(cr)
