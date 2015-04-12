
##
# Plot the histogram of the Global Active Power.
#
# Inputs:
# raw = raw data of the "household_power_consumption.txt" file
#
# Reading the raw data:
# read.table("household_power_consumption.txt", header=TRUE, sep=";",
#     na.strings = "?",
#     colClasses=c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
##

plot1 <- function(raw) {
  
  # Filtering the data by the date
  rawDates <- as.Date(raw$Date, format="%d/%m/%Y")
  minDateFilter <- as.Date("01/02/2007", format="%d/%m/%Y")
  maxDateFilter <- as.Date("02/02/2007", format="%d/%m/%Y")
  
  dateFiltered <- raw[rawDates >= minDateFilter & rawDates <= maxDateFilter,]
  
  # Plotting and saving to disk
  hist(as.numeric(dateFiltered$Global_active_power), col="red", xlab="Global Active Power (kilowatts)", main="")
  title(main = "Global Active Power")
  dev.copy(png, file = "plot1.png")
  dev.off()
}