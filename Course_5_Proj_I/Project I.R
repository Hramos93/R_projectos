#Load data
library(dplyr)
library(data.table)
library(magrittr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("hrbrthemes")
library(hrbrthemes)

path = "~/Project/R/Data Science/R/Projectos/R_projectos/repdata_data_activity/activity.csv"

data <- read.csv(path,header=TRUE,sep=",")
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

 
 intervals[which(intervals$steps == max (intervals$steps)),]
 
 #IMPUTING MISSING VALUES
 
 #CALCULATE AND REPORT THE TOTAL NUMBER OF MISSING VALUES IN THE DATASET
 
missing <-sum(is.na(data))
missing
 
replacewithmean <- function(x)
  replace(x, is.na(x), mean(x, na.rm=TRUE))

meandata <- data %>% group_by(interval) %>%
  mutate(steps = replacewithmean(steps))

head(meandata)

# Make 

FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)
                                    

summary(FullSummedDataByDay)    

ggplot(FullSummedDataByDay, aes(x=totalsteps, fill = cut(x=totalsteps, 100)))+
  geom_histogram(show.legend=FALSE)+
  ggtitle("Total Daily Steps")
 