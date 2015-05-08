path = 'exdata-data-household_power_consumption/household_power_consumption.txt'

data = read.table(path, header = TRUE, sep = ";", na.strings = "?")

names(data)

data$Date <- as.Date(data$Date, "%d/%m/%Y")

data <- data[data$Date %in% as.Date(c("2007-02-01", "2007-02-02"),"%Y-%m-%d"),]

data$Time <- strptime(paste(data$Date, data$Time), format = "%Y-%m-%d %H:%M:%S")

png("plot2.png", width = 480, height = 480)

plot(data$Time, data$Global_active_power, xlab = "", ylab = "Global active power (kilowatts)", "l")

dev.off()
