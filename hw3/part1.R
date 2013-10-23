outcome <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv",
                    colClasses = 'character')
outcome[,11] <- as.numeric(outcome[,11])

hist(outcome[,11], xlab='30-day Death Rate',
     main='Heart Attack 30-day Death Rate')