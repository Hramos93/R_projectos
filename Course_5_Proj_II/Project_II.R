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

#property damage

unique(data$PROPDMGEXP)

data$PROPDMGEXP <- as.character(data$PROPDMGEXP)
data$PROPDMGEXP[is.na(data$PROPDMGEXP)] <- 0
data$PROPDMGEXP[!grepl("H|K|M|B", data$PROPDMGEXP,ignore.case = TRUE)] <- 0
data$PROPDMGEXP <- gsub("H|h","2", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("K|k","3", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("M|m","6", data$PROPDMGEXP)
data$PROPDMGEXP <- gsub("B|b","9", data$PROPDMGEXP)
data$PROPDMGEXP <- as.numeric(as.character(data$PROPDMGEXP))
data$propertyDMG <- data$PROPDMG * 10^data$PROPDMGEXP

unique(data$CROPDMGEXP)

data$CROPDMGEXP <- as.character(data$CROPDMGEXP)
data$CROPDMGEXP[is.na(data$CROPDMGEXP)] <- 0
data$CROPDMGEXP[!grepl("H|K|M|B", data$CROPDMGEXP,ignore.case = TRUE)] <- 0
data$CROPDMGEXP <- gsub("H|h","2", data$CROPDMGEXP)
data$CROPDMGEXP <- gsub("K|k","3", data$CROPDMGEXP)
data$CROPDMGEXP <- gsub("M|m","6", data$CROPDMGEXP)
data$CROPDMGEXP <- gsub("B|b","9", data$CROPDMGEXP)
data$CROPDMGEXP <- as.numeric(as.character(data$CROPDMGEXP))
data$CROP.DMG <- data$CROPDMG * 10^data$CROPDMGEXP

sort(table(data$propertyDMG), decreasing = TRUE)[1:10]