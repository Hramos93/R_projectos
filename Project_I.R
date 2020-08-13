library(dlpyr)
library(data.table)
library(ggplot2)

list.files("./data")

files = list.files("./data", recursive=TRUE)
pathdata = file.path("./data")
df = read.table(file.path(pathdata,  "household_power_consumption.txt"), header=TRUE)

