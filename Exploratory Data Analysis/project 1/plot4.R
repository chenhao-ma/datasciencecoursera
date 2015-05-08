path = 'exdata-data-household_power_consumption/household_power_consumption.txt'

data = read.table(path, header = TRUE, sep = ";", na.strings = "?")

names(data)

data$Date <- as.Date(data$Date, "%d/%m/%Y")

data <- data[data$Date %in% as.Date(c("2007-02-01", "2007-02-02"),"%Y-%m-%d"),]

data$Time <- strptime(paste(data$Date, data$Time), format = "%Y-%m-%d %H:%M:%S")

png("plot4.png", width = 480, height = 480)

par(mfrow=c(2,2))

plot(data$Time, data$Global_active_power, xlab = "", ylab = "Global active power (kilowatts)", "l")

plot(data$Time, data$Voltage, xlab = "datetime", ylab = "Voltage", "l")

plot(data$Time, data$Sub_metering_1, xlab = "", ylab = "Energy sub metering", type = "l", col = "red")
lines(data$Time, data$Sub_metering_2, col = "brown")
lines(data$Time, data$Sub_metering_3, col = "blue")
legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
        col=c("red", "brown", "blue"), lty = 1, bty='n', cex=.75)

plot(data$Time, data$Global_reactive_power, xlab = "datetime", ylab = "Global_reactive_power", "l")

dev.off()
