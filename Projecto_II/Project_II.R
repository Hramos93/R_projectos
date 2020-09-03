library(data.table)


NEI <- readRDS("./Projecto_II/summarySCC_PM25.rds")
SCC <- readRDS("./Projecto_II/Source_Classification_Code.rds")

head(NEI)
summary(NEI)
str(NEI)
sum(is.na(NEI))


head(SCC)
summary(SCC)

head(NEI$year)