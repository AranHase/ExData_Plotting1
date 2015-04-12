
##
# Plot the Global Active Power by Time.
#
# Inputs:
# raw = raw data of the "household_power_consumption.txt" file
#
# Reading the raw data:
# read.table("household_power_consumption.txt", header=TRUE, sep=";",
#     na.strings = "?",
#     colClasses=c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
##

plot3 <- function(raw) {
  
  # Filtering the data by the date
  rawDates <- as.Date(raw$Date, format="%d/%m/%Y")
  minDateFilter <- as.Date("01/02/2007", format="%d/%m/%Y")
  maxDateFilter <- as.Date("02/02/2007", format="%d/%m/%Y")
  
  dateFiltered <- raw[rawDates >= minDateFilter & rawDates <= maxDateFilter,]
  
  # Join the Date and Time columns
  dateFiltered$DateTime <- (sapply(dateFiltered, function(x) paste(dateFiltered[,1], dateFiltered[, 2], sep=" "))[,1])  
  dateFiltered$DateTime <- strptime(dateFiltered$DateTime, format="%d/%m/%Y %H:%M:%S")
  
  # Plotting and saving to disk
  
  png(file = "plot3.png")
  plot(dateFiltered$DateTime, dateFiltered$Sub_metering_1, type="n", xlab="", ylab="Energy sub metering")
  lines(dateFiltered$DateTime, dateFiltered$Sub_metering_1)
  lines(dateFiltered$DateTime, dateFiltered$Sub_metering_2, col = "red")
  lines(dateFiltered$DateTime, dateFiltered$Sub_metering_3, col = "blue")
  #
  legend("topright", legend = names(raw)[7:9], lty=c(1,1,1), col=c("black", "red", "blue"), cex=1, xpd=NA)
  
  title(main = "")
  # dev.copy doesn't work for this plot because of the legend. `dev.copy` fails to calculate the proper width :(
  #dev.copy(png, file = "plot3.png")
  dev.off()
}