rankhospital <- function(state, outcome, num = "best") {
  outcomes <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
  #check that outcome is valid
  if(!outcome%in%names(outcomes)) {stop("invalid outcome")}
  #interpret outcome to column number
  outcome <- outcomes[outcome]
  
  #read in outcome data
  data <- read.csv('ProgAssignment3-data/outcome-of-care-measures.csv', colClasses='character')
  #check that outcome is valid
  if(!state%in%data$State) {stop("invalid state")}
  
  #extract outcome data of the given state
  data_of_state <- data[data$State == state, ]
  #suppress the warnings of creating NAs in transforming data type
  data_of_state[, outcome] <- suppressWarnings(as.numeric(data_of_state[, outcome]))
  #remove NAs
  data_of_state <- data_of_state[!is.na(data_of_state[, outcome]),]
  
  rank <- if(is.numeric(num)) {
    num
  } else if (num == 'best') {
    1
  } else if (num == 'worst') {
    length(as.factor(data_of_state[, 1]))
  } else if (!is.numeric(as.integer(num))) {
    as.integer(num)
  } else {
    stop('invalid rank number')
  }
  
  data_of_state.ordered <- data_of_state[order(data_of_state[, outcome], data_of_state$Hospital.Name), ]
  
  # Return hospital name in the state with the given rank
  data_of_state.ordered[rank, 'Hospital.Name']
}