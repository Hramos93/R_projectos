#Libraries
library(dplyr)
library(data.table)
library(magrittr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("hrbrthemes")
library(tidyverse)
library(hrbrthemes)
#install.packages("viridis")
library(viridis)

#Load data
path = "~/Project/R/Data Science/R/Projectos/R_projectos/repdata_data_activity/activity.csv"
data <- read.csv(path,header=TRUE,sep=",")

summary(data)

#Table sumarry of steps  by day
Measures <- data %>% select(date,steps) %>% group_by(date) %>%
  summarize(stepsbyday = sum(steps , na.rm=TRUE)) 

summary(Measures)

#plot steps frecuency steps by day
ggplot(Measures, aes(x=stepsbyday, fill = cut(x=stepsbyday, 100)))+
  geom_histogram(show.legend=FALSE)+
  ggtitle("Frequency Steps")

##########MEASURES##################3333
meanPreNa <- round(mean(Measures$stepsbyday),digits = 2)
medianPreNA <- round(median(Measures$stepsbyday),digits = 2)


#Tablw with frequency steps by interval
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
 
 #WITH MEAN
 
missing <-sum(is.na(data))
missing
 
replacewithmean <- function(x)
  replace(x, is.na(x), mean(x, na.rm=TRUE))

meandata <- data %>% group_by(interval) %>%
  mutate(steps = replacewithmean(steps))

summary(meandata)

#Plot frequency steps 
FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)
                                    

summary(FullSummedDataByDay)    

ggplot(FullSummedDataByDay, aes(x=totalsteps, fill = cut(x=totalsteps, 100)))+
  geom_histogram(show.legend=FALSE)+
  ggtitle("Total Daily Steps")


#COMPUTE NEW MEASURES DATA 

meanPost <- round(mean(FullSummedDataByDay$totalsteps), digits = 2)
medianPost <- round(median(FullSummedDataByDay$totalsteps), digits = 2)



Compare <- data.frame(mean = c(meanPreNa,meanPost),median = c(medianPreNA,medianPost))
rownames(Compare) <- c("Pre NA ", "Post NA ")
print(Compare)

#Plot Compare by stage week

meandata$date <- as.Date(meandata$date)
meandata$weekday <- weekdays(meandata$date)
meandata$weekend <- ifelse(meandata$weekday=="sábado" | meandata$weekday=="domingo", "Weekend", "Weekday" )

meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")

ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
  facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
  ggtitle("Comparison of Average Number of Steps in Each Interval")