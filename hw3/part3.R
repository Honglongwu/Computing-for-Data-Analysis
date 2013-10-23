outcome <- read.csv('ProgAssignment3-data/outcome-of-care-measures.csv', colClasses='character')
outcome[,11] <- as.numeric(outcome[,11])
hos_count <- table(outcome$State)
hos_count <- hos_count[hos_count>=20]
outcome2 <- outcome[outcome$State %in% names(hos_count), ]

death <- outcome2[,11]
state <- outcome2$State
boxplot(death ~ state, 
        main='Heart Attack 30-day Death Rate by State',
        ylab='30-day Death Rate',
        las=2)

death_rate_medians <- tapply(outcome2[,11], as.factor(outcome2$State), median, na.rm=T)

boxplot(outcome2[,11] ~ factor(outcome2$State,levels=names(sort(death_rate_medians))), 
        main='Heart Attack 30-day Death Rate by State',
        ylab='30-day Death Rate',
        las=2)