
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

plot2 <- function(raw) {
  
  # Filtering the data by the date
  rawDates <- as.Date(raw$Date, format="%d/%m/%Y")
  minDateFilter <- as.Date("01/02/2007", format="%d/%m/%Y")
  maxDateFilter <- as.Date("02/02/2007", format="%d/%m/%Y")
  
  dateFiltered <- raw[rawDates >= minDateFilter & rawDates <= maxDateFilter,]
  
  # Join the Date and Time columns
  dateFiltered$DateTime <- (sapply(dateFiltered, function(x) paste(dateFiltered[,1], dateFiltered[, 2], sep=" "))[,1])  
  dateFiltered$DateTime <- strptime(dateFiltered$DateTime, format="%d/%m/%Y %H:%M:%S")
  
  # Plotting and saving to disk
  
  ## plot the axis, titles, etc, but WITHOUT plotting points
  plot(dateFiltered$DateTime, dateFiltered$Global_active_power, type="n", xlab="", ylab="Global Active Power (killowatts)")
  lines(dateFiltered$DateTime, dateFiltered$Global_active_power)
  title(main = "")
  dev.copy(png, file = "plot2.png")
  dev.off()
}