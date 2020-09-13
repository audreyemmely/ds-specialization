rankhospital <- function(state, outcome, num) {
  
  
  ## Read outcome data .csv file
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  ## All of the possible outcome strings
  outcomes = c("heart attack", "heart failure", "pneumonia")
  ## Check if outcome is one of the strings in outcomes
  if( outcome %in% outcomes == FALSE ) {
    stop("invalid outcome")
  }
  
  ## Validate the state string
  ## All of the possible states from the data
  states <- data[, 2]
  states <- unique(states)
  ## Check if state is one of the states in the data
  if( state %in% states == FALSE ) {
    stop("invalid state")
  }
  
  ## Validate the num value by checking if it is "best","worst", or a number.
  ## NOTE: If num was Boolean then num%%1 = 0
  if( num != "best" && num != "worst" && num%%1 != 0 ) {
    stop("invalid num")
  }
  
  ## Get only the rows with our state value    
  data <- data[data$state==state & data[outcome] != 'Not Available', ]
  
  ## Order the data by name and then outcome
  data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
  data <- data[order(data$name, decreasing = FALSE), ]
  data <- data[order(data[outcome], decreasing = FALSE), ]
  
  ## Process the num argument to get the row index
  vals <- data[, outcome]
  if( num == "best" ) {
    rowNum <- which.min(vals)
  } else if( num == "worst" ) {
    rowNum <- which.max(vals)
  } else {
    rowNum <- num
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  data[rowNum, ]$name
}