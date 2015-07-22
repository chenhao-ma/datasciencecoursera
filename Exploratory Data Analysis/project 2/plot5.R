#import data

NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

#subset SCC to get the required SCC code using regex

CodeSCC <- SCC[grep("Mobile.*Vehicles",SCC$EI.Sector,value = F),]
CodeSCC <- CodeSCC$SCC
CodeSCC <- as.vector(CodeSCC)

#subset NEI to get the data about mobile vehicles and in Baltimore

NEIVeh <- subset(NEI, (NEI$SCC %in% CodeSCC) & (NEI$fips == "24510"), select = c(Emissions, year))

#aggregate the data by the year

EmiPerYear <- aggregate(NEIVeh$Emissions, by = list(year = NEIVeh$year), sum)

#plot to file plot5.png

png(file = "plot5.png")

ggplot(data = EmiPerYear) + geom_line(aes(x=year, y=x)) + labs(title = "Emissions from 1999–2008 for Baltimore City about motor vehicle") + labs(x = "year") + labs(y = "Emissions (in tons)")

dev.off()
