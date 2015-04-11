rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  
  out <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  out[, 11] <- as.numeric(out[, 11])
  out[, 17] <- as.numeric(out[, 17])
  out[, 23] <- as.numeric(out[, 23])
  
  ## Check that state and outcome are valid
  
  if (!is.element(state, out[["State"]]))
    stop("invalid state")
  validOutcome <- c("heart attack", "heart failure", "pneumonia")
  rateName <- c(11,17,23)    
  if (!is.element(outcome, validOutcome))
    stop("invalid outcome")
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  pos <- match(outcome, validOutcome)  
  subOut <- subset(out, State==state ,select=c(2,rateName[pos]))
  subOut <- subOut[!is.na(subOut[,2]),]
  
  if (num=="best")
    num = 1L
  if (num=="worst")
    num = nrow(subOut)
  subOut <- subOut[order(subOut[,2],subOut[,1]),]
  
  if (num>nrow(subOut))
    return (NA)
  else
    return (subOut[num,1][[1]])
  
}