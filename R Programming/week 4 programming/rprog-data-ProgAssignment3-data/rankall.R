rankall <- function(outcome, num = "best") {
  ## Read outcome data
  
  out <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  out[, 11] <- as.numeric(out[, 11])
  out[, 17] <- as.numeric(out[, 17])
  out[, 23] <- as.numeric(out[, 23])
  
  ## Check that state and outcome are valid
  
  validOutcome <- c("heart attack", "heart failure", "pneumonia")
  rateName <- c(11,17,23)    
  if (!is.element(outcome, validOutcome))
    stop("invalid outcome")
  
  allST <- out[[7]]
  allST <- unique(allST)
  allST <- sort(allST)
  
  ## For each state, find the hospital of the given rank
  
  ret <- data.frame(hospital<-c(),state<-c())
  
  
  for (state in allST){
    pos <- match(outcome, validOutcome)  
    subOut <- subset(out, State==state ,select=c(2,rateName[pos]))
    subOut <- subOut[!is.na(subOut[,2]),]
    
    if (num=="best")
      n = 1L
    else if (num=="worst")
      n = nrow(subOut)
    else n = num
    
    subOut <- subOut[order(subOut[,2],subOut[,1]),]
    
    if (n>nrow(subOut))
      tmp <- data.frame(hospital<-c(NA),state<-c(state))
    else
      tmp <- data.frame(hospital<-subOut[n,1][[1]],state<-c(state))
    names(tmp)<-names(ret)
    ret <- rbind(ret,tmp)
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  names(ret) <- c("hospital","state")
  row.names(ret) <- allST
  ret
}