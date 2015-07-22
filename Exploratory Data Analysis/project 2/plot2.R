#import data

NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

#subset NEI to get the data about Baltimore City

NEIBal <- subset(NEI, NEI$fips == "24510", select = c(Emissions, year))

#aggregate the data by the year

EmiPerYear <- aggregate(NEIBal$Emissions, by = list(year = NEIBal$year), sum)

#plot the Scatterplot to file plot2.png

png(file = "plot2.png")

plot(EmiPerYear$year, EmiPerYear$x, xlab = "year", ylab = "Emissions (in tons)")
title(main = "The total PM2.5 emission in Baltimore City per year")

dev.off()
