rankall <- function(outcome, num = "best") {
  outcomes <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
  
  #check that outcome is valid
  if(!outcome%in%names(outcomes)) {stop("invalid outcome")}
  #interpret outcome to column number
  outcome <- outcomes[outcome]
  
  #read in outcome data
  data <- read.csv('ProgAssignment3-data/outcome-of-care-measures.csv', colClasses='character')
  #suppress the warnings of creating NAs in transforming data type
  data[, outcome] <- suppressWarnings(as.numeric(data[, outcome]))
  #remove NAs
  data <- data[!is.na(data[, outcome]),]
  
  #split the data by state
  data.states <- split(data, as.factor(data$State))
  #order outcomes of each states by death rate and hospital' name
  data.states.ordered <- sapply(data.states, function(x){x[order(x[,outcome], x$Hospital.Name),]})
                        
  #return the name of hospital that has the given rank at each state
  hospitals_of_rank <- sapply(data.states.ordered['Hospital.Name', ], function(x){
    rank <- if(is.numeric(num)) {
      num
    } else if (num == 'best') {
      1
    } else if (num == 'worst') {
      length(x)
    } else if (!is.numeric(as.integer(num))) {
      as.integer(num)
    } else {
      stop('invalid rank number')
    }
    x[rank]
  })
  
  #return the data frame
  data.frame(hospital=hospitals_of_rank, state=names(hospitals_of_rank))
}