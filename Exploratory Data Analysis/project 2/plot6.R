#import data

NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

#subset SCC to get the required SCC code using regex

CodeSCC <- SCC[grep("Mobile.*Vehicles",SCC$EI.Sector,value = F),]
CodeSCC <- CodeSCC$SCC
CodeSCC <- as.vector(CodeSCC)

#subset NEI to get the data about mobile vehicles and in Baltimore and Los Angeles County

NEIVeh <- subset(NEI, (NEI$SCC %in% CodeSCC) & (NEI$fips %in% c("06037", "24510")), select = c(Emissions, year, fips))

f <- function(x){
  if (x == "06037")
    temp <- "Los Angeles County"
  else
    temp <- "Baltimore City"
  temp
}

#aggregate the data by the year

EmiPerYear <- aggregate(NEIVeh$Emissions, by = list(year = NEIVeh$year, fips = NEIVeh$fips), sum)

#add city attribute
EmiPerYear$city <- lapply(EmiPerYear$fips, f)
EmiPerYear$city <- rapply(EmiPerYear$city,c)

#plot to file plot6.png

png(file = "plot6.png")

ggplot(data = EmiPerYear) + geom_line(aes(x=year, y=x, color = city)) + facet_grid(city ~ ., scales = "free", space = "free") + labs(title = "Emissions in Baltimore & Los Angeles about motor vehicle") + labs(x = "year") + labs(y = "Emissions (in tons)")

dev.off()
