library(ggplot2)
library(dplyr)
library(reshape2)
library(grid)
library(gridExtra)


if(!exists("storm.data")) {
  storm.data <- read.csv(bzfile("repdata_data_StormData.csv.bz2"),header = TRUE)
}

dim(storm.data)
str(storm.data)

var <- c("BGN_DATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")
data <- storm.data[, var]


head(data)
tail(data)

data$BGN_DATE <- format(as.Date(data$BGN_DATE, "%m/%d/%Y %H :%M :%S"), "%Y")

data$BGN_DATE = as.numeric(levels(data$BGN_DATE))[data$BGN_DATE]
str(data)

unique(data$PROPDMGEXP)

data$PROPDMGEXP <- as.character(data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("B","9", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("H|h","2", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("K","3", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("M|m","6", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("B","9", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("-|?|+","0", data$PROPDMGEXP)

data$PROPDMGEXP <- as.numeric(data$PROPDMGEXP)

summary(data)

INJURIES <- aggregate(data$INJURIES, by = list(EVTYPE = data$EVTYPE), sum) 
INJURIES <- INJURIES[order(INJURIES$x, decreasing= TRUE),]
names(INJURIES)[2] <- "totalInjuries"

p1 <- ggplot(INJURIES[1:5, ], aes(x=reorder(EVTYPE, -totalInjuries), y =totalInjuries, fill = EVTYPE)) +
  geom_bar(stat = "identity")+
  xlab("Event Type") + 
  ylab("Number of Injuries")+
  ggtitle("Injuries by Event type")+
  theme(axis.text.x=element_blank(),legend.title = element_text(color = "blue", size = 5),
        legend.text = element_text(color = "red", size = 5))
 

#FATALITIES

FATALITIES <- aggregate(data$FATALITIES, by = list(EVTYPE = data$EVTYPE), sum) 
FATALITIES <- FATALITIES[order(FATALITIES$x, decreasing= TRUE),]
names(FATALITIES)[2] <- "totalFatal"

p2 <- ggplot(FATALITIES[1:5, ], aes(x=reorder(EVTYPE, -totalFatal), y =totalFatal, fill = EVTYPE)) +
  geom_bar(stat = "identity")+
  xlab("Event Type") + 
  ylab("Number of Injuries")+
  ggtitle("Injuries by Event type")+
  theme(axis.text.x=element_blank(),legend.title = element_text(color = "blue", size = 5),
        legend.text = element_text(color = "red", size = 5))

grid.arrange(p1, p2, ncol = 2, top = "Plot")