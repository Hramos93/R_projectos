library(dlpyr)
library(data.table)
library(ggplot2)

list.files("../data")

files = list.files("../data", recursive=TRUE)
pathdata = file.path("../data")
df = read.table(file.path(pathdata,  "household_power_consumption.txt"), header=TRUE,sep=';', na.string="?")


df_seg <- subset(df, Date %in% c("1/2/2007", "2/2/2007"))

#PLOT 2_

datime <- strptime(paste(df_seg$Date, df_seg$Time, sep=""), "%d/%m/%Y %H:%M:%S")
data <- as.numeric(df_seg$Global_active_power)
png("plot2.png", width=480, height=480)
plot(datime, data, type="l", xlab="", ylab="Global Active Power (Pw)")
dev.off()
