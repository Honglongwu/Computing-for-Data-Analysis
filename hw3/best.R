best <- function(state, outcome){
  outcomes <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
  
  #check that outcome is valid
  if(!outcome%in%names(outcomes)) {stop("invalid outcome")}
  #interpret outcome to column number
  outcome <- outcomes[outcome]
  
  #read in outcome and hospital data
  data <- read.csv('ProgAssignment3-data/outcome-of-care-measures.csv', colClasses='character')
  hospital <- read.csv('ProgAssignment3-data/hospital-data.csv', colClasses='character')
  
  #check that outcome is valid
  if(!state%in%data$State) {stop("invalid state")}
  
  #extract outcome data of the given state
  data_of_state <- data[data$State == state, ]
  #Transform data type from character to numeric and suppress the warnings of NAs created
  data_of_state[, outcome] <- suppressWarnings(as.numeric(data_of_state[, outcome]))
  
  #remove NAs
  data_of_state <- data_of_state[!is.na(data_of_state[, outcome]),]
  
  #Provider.Numbers of the hospital with the lowest death rate
  min_death_rate <- min(data_of_state[, outcome])
  provider_numbers <- data_of_state[data_of_state[, outcome] == min_death_rate, 1] 
  
  #Handling ties. hospital names should be sorted in alphabetical order and
  #the first hospital in that set should be chosen
  hos_names <- hospital[hospital$Provider.Number%in%provider_numbers, 2]
  hos_names <- sort(hos_names)
  hos_names[1]
}