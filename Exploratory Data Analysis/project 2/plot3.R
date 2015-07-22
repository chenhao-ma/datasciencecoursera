#import data

NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

#subset NEI to get the data about Baltimore City

NEIBal <- subset(NEI, NEI$fips == "24510", select = c(Emissions, year, type))

#aggregate the data by the year and type

EmiPerYear <- aggregate(NEIBal$Emissions, by = list(year = NEIBal$year, type = NEIBal$type), sum)

#plot to file plot3.png

png(file = "plot3.png")

ggplot(data = EmiPerYear) + geom_line(aes(x=year, y=x, colour=type)) + labs(title = "Emissions from 1999–2008 for Baltimore City in four type") + labs(x = "year") + labs(y = "Emissions (in tons)")

dev.off()
