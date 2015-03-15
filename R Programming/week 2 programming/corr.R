corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ans <- c()
  for (i in 1:332){
    if (i<10){
      name=paste("00",i,sep="")
    }else if (i<100){
      name=paste("0",i,sep="")
    }else{
      name=toString(i)
    }    
    dir <- paste(directory,"/",name,".csv",sep="")
    pol <- read.csv(dir);
    x <- subset(pol,complete.cases(pol))[[2]]
    y <- subset(pol,complete.cases(pol))[[3]]
    if (length(x)>threshold){
      ans<-c(ans,c(cor(x,y)))
    }
  }
  
  return (ans)
}