#import data

NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

#subset SCC to get the required SCC code using regex

CodeSCC <- SCC[grep("Fuel Comb.*Coal",SCC$EI.Sector,value = F),]
CodeSCC <- CodeSCC$SCC
CodeSCC <- as.vector(CodeSCC)

#subset NEI to get the data about coal combustion

NEICoal <- subset(NEI, NEI$SCC %in% CodeSCC, select = c(Emissions, year))

#aggregate the data by the year

EmiPerYear <- aggregate(NEICoal$Emissions, by = list(year = NEICoal$year), sum)

#plot to file plot4.png

png(file = "plot4.png")

ggplot(data = EmiPerYear) + geom_line(aes(x=year, y=x)) + labs(title = "Emissions from 1999–2008 for U.S. about coal combustion") + labs(x = "year") + labs(y = "Emissions (in tons)")

dev.off()
