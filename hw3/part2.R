outcome <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv",
                    colClasses = 'character')
outcome[,11] <- as.numeric(outcome[,11])
outcome[,17] <- as.numeric(outcome[,17])
outcome[,23] <- as.numeric(outcome[,23])
par(mfrow=c(3,1))
hist(outcome[,11], xlab='30-day Death Rate', probability=T,
     main=bquote(.('Heart Attack (') ~ bar(x) == .(median(outcome[,11], na.rm=T)) ~ ')'))
abline(v=median(outcome[,11], na.rm=T), col='red')
lines(density(outcome[,11], na.rm=T), col='blue')

hist(outcome[,17], xlab='30-day Death Rate', probability=T,
     main=bquote(.('Heart Failure (') ~ bar(x) == .(median(outcome[,11], na.rm=T)) ~ ')'))
abline(v=median(outcome[,17], na.rm=T), col='red')
lines(density(outcome[,17], na.rm=T), col='blue')

hist(outcome[,23], xlab='30-day Death Rate', probability=T,
     main=bquote(.('Pneumonia (') ~ bar(x) == .(median(outcome[,11], na.rm=T)) ~ ')'))
abline(v=median(outcome[,23], na.rm=T), col='red')
lines(density(outcome[,23], na.rm=T), col='blue')