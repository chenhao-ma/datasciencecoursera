#import data

NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

#aggregate the data by the year

EmiPerYear <- aggregate(NEI$Emissions, by = list(year = NEI$year), sum)

#plot the Scatterplot to file plot1.png

png(file = "plot1.png")

plot(EmiPerYear$year, EmiPerYear$x, xlab = "year", ylab = "Emissions (in tons)")
title(main = "The total PM2.5 emission in U.S. per year")

dev.off()
