#Load data
library(dplyr)
library(data.table)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("hrbrthemes")
library(hrbrthemes)

path = "~/Project/R/Data Science/R/Projectos/R_projectos/repdata_data_activity/activity.csv"

data <- read.csv(path,header=TRUE,sep=",")
summary(data)
data <- data[- which(is.na(data$steps)),]
summary(data)


Measures <- data %>% select(date,steps) %>% group_by(date) %>%
  summarize(stepsbyday = sum(steps)) %>% na.omit()

ggplot(Measures, aes(x=stepsbyday, fill = cut(x=stepsbyday, 100)))+
  geom_histogram(show.legend=FALSE)+
  ggtitle("Frequency Steps")


mean(Measures$stepsbyday)
median(Measures$stepsbyday)

intervals <- data %>% select(interval,steps) %>%  na.omit() %>%
  group_by(interval) %>% summarize(steps = mean(steps))

 ggplot(intervals, aes(x=interval, y=steps)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
   ggtitle("mean-Steps")
