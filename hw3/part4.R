library(lattice)

outcome <- read.csv('ProgAssignment3-data/outcome-of-care-measures.csv', colClasses='character')
hospital <- read.csv('ProgAssignment3-data/hospital-data.csv', colClasses='character')
outcome.hospital <- merge(outcome, hospital, by='Provider.Number')
death <- as.numeric(outcome.hospital[, 11]) ##Heart attack outcome
npatient <- as.numeric(outcome.hospital[, 15])
owner <- factor(outcome.hospital$Hospital.Ownership)

xyplot(death ~ npatient | owner, 
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.lmline(x, y)
       },
       main='Heart Attack 30-day Death Rate by Ownership',
       xlab='Number of Patients Seen',
       ylab='30-day Death Rate')