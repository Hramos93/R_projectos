library(data.table)
library(tidyverse)
library(readr)

NEI <- readRDS("./Projecto_II/summarySCC_PM25.rds")
SCC <- readRDS("./Projecto_II/Source_Classification_Code.rds")

head(NEI)
summary(NEI)
str(NEI)
sum(is.na(NEI))


head(SCC)
summary(SCC)


# Aggregate by sum the total emissions by year
aggTotals <- aggregate(Emissions ~ year,NEI, sum)

png("plot1.png",width=480,height=480,units="px",bg="transparent")

barplot(
  (aggTotals$Emissions)/10^6,
  names.arg=aggTotals$year,
  xlab="Year",
  ylab="PM2.5 Emissions (10^6 Tons)",
  main="Total PM2.5 Emissions From All US Sources"
)

dev.off()