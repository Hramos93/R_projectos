library(dlpyr)
library(data.table)
library(ggplot2)

list.files("../data")

files = list.files("../data", recursive=TRUE)
pathdata = file.path("../data")
df = read.table(file.path(pathdata,  "household_power_consumption.txt"), header=TRUE,sep=';', na.string="?")


df_seg <- subset(df, Date %in% c("1/2/2007", "2/2/2007"))

#PLOT 4_
datetime <- strptime(paste(df_seg$Date, df_seg$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 
globalActivePower <- as.numeric(df_seg$Global_active_power)
subMetering1 <- as.numeric(df_seg$Sub_metering_1)
subMetering2 <- as.numeric(df_seg$Sub_metering_2)
subMetering3 <- as.numeric(df_seg$Sub_metering_3)

voltage <- as.numeric(df_seg$Voltage)
globalReactivePower <- as.numeric(df_seg$Global_reactive_power)


png("plot4.png", width=480, height=480)
par(mfrow = c(2, 2))
#global active power
plot(datetime, globalActivePower, type="l", xlab="", ylab="Global Active Power", cex=0.2)
#voltage
plot(datetime, voltage, type="l", xlab="datetime", ylab="Voltage")
#Energy Submetering
plot(datetime, subMetering1, type="l", ylab="Energy Submetering", xlab="")
lines(datetime, subMetering2, type="l", col="red")
lines(datetime, subMetering3, type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, lwd=2.5, col=c("black", "red", "blue"))

#global reactive Power
plot(datetime, globalReactivePower, type="l", xlab="datetime", ylab="Global_reactive_power")
dev.off()