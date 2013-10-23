best <- function(state, outcome){
  state <- 'AL'
  outcome <- 'heart attack'
  outcomes <- c(11,17,23)
  names(outcomes) <- c('heart attack','heart failure','pneumonia')
  data <- read.csv('ProgAssignment3-data/outcome-of-care-measures.csv', colClasses='character')
  hospital <- read.csv('ProgAssignment3-data/hospital-data.csv', colClasses='character')
  
  data_of_state <- data[data$State == state, ]
  data_of_state[,outcomes[outcome]] <- as.numeric(data_of_state[, outcomes[outcome]])
  #remove NAs
  data_of_state <- data_of_state[!is.na(data_of_state[,outcomes[outcome]]),]
  #Provider.Numbers of the hospital with the lowest death rate
  min_death_rate <- min(data_of_state[, outcomes[outcome]])
  provider_numbers <- data_of_state[data_of_state[, outcomes[outcome]] == min_death_rate, 1] 
  
  #return names of hospitals
  hospital[hospital$Provider.Number%in%provider_numbers, 2]
}