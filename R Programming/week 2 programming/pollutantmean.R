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
  
  x<-vector("numeric",length=length(id))
  num<-vector("integer",length=length(id))
  for (i in id){
    if (i<10){
      name=paste("00",i,sep="")
    }else if (i<100){
      name=paste("0",i,sep="")
    }else{
      name=toString(i)
    }
      
    dir<-paste(directory,"/",name,".csv",sep="")
    pol<-read.csv(dir)
    y<-subset(pol,select=pollutant)[[1]]
    bad <- is.na(y)
    y<- y[!bad]
    x[i-id[1]+1]<-sum(y)
    num[i-id[1]+1]<-length(y)
  }
  bad=is.na(x)
  x<- x[!bad]
  num<- num[!bad]  
  return(sum(x)/sum(num))
  
}