library(lubridate)
library(ggplot2)
library(dplyr)
air1999<-read.delim("Airpollution_1999.txt", sep="|")
air1999 <- air1999[2:nrow(air1999),]

for (i in c(1,2,3,4,5,6,7,10,14:28)) {
  air1999[,i] <- as.factor(air1999[,i])
}

for (i in c(8,9,13)) {
  air1999[,i] <- as.numeric(air1999[,i])
}

air1999$Date <-  ymd(air1999$Date)
air1999$Start.Time <- ifelse(is.na(air1999$Start.Time), NA, hm(air1999$Start.Time))

ggplot(air1999) +
  geom_histogram(mapping=aes(x=Sample.Value))
summary(air1999$Sample.Value)
summary(air1999$Sample.Duration) # all durations are the same bar one.

states_group <- air1999 %>% group_by(State.Code) %>% summarise(ave.sample = mean(Sample.Value, na.rm = TRUE))
ggplot(states_group) +
  geom_col(mapping=aes(x=State.Code, y=ave.sample))

ggplot(states_group) +
  geom_boxplot(mapping=aes(y=ave.sample))

sqrt(var(states_group$ave.sample,na.rm = TRUE))
