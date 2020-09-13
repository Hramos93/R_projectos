#Load data
library(dplyr)
library(data.table)
#install.packages("ggplot2")
library(ggplot2)

path = "~/Project/R/Data Science/R/Projectos/R_projectos/repdata_data_activity/activity.csv"

data <- read.csv(path,header=TRUE,sep=",")
summary(data)
data <- data[- which(is.na(data$steps)),]
summary(data)


stepsbyday <- data %>% select(date,steps) %>% group_by(date) %>%
  summarize(tsteps = sum(steps)) %>% na.omit()

ggplot(stepsbyday, aes(x=tsteps, fill = cut(x=tsteps, 100)))+
  geom_histogram(show.legend=FALSE)+
  ggtitle("Frequency Steps")


data %>%
  group_by(date,steps)
