
path = 'exdata-data-household_power_consumption/household_power_consumption.txt'

data = read.table(path, header = TRUE, sep = ";", na.strings = "?")

names(data)

data$Date <- as.Date(data$Date, "%d/%m/%Y")

data <- data[data$Date %in% as.Date(c("2007-02-01", "2007-02-02"),"%Y-%m-%d"),]

png("plot1.png", width = 480, height = 480)

hist(data$Global_active_power, main = "Global Active Power", xlab = 'Global Active Power (kilowatts)', ylab = "Frequency", col = 'red')

dev.off()



