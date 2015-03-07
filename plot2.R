readDataFile <- function(fileName) {
        ## reads Electric power consumption file
        ## it is big but not too big
        hpc <- read.table(fileName, header=TRUE, sep=";", stringsAsFactors=FALSE)
        hpc
}

extractSubSet <- function(hpc) {
        ## Limits to the desired two dates
        ## No ? in this subset noticed
        hpc$DateOnly <- as.Date(hpc$Date, "%d/%m/%Y")
        hpc_s <- subset(hpc, DateOnly == as.Date("2007-02-01", "%Y-%m-%d") | DateOnly == as.Date("2007-02-02", "%Y-%m-%d"))
        
        hpc_s$DateTime <- strptime(paste(hpc_s$Date, hpc_s$Time), "%d/%m/%Y %H:%M:%OS")
        hpc_s$Global_active_power <- as.numeric(hpc_s$Global_active_power)
        hpc_s$Global_reactive_power <- as.numeric(hpc_s$Global_reactive_power)
        hpc_s$Voltage <- as.numeric(hpc_s$Voltage)
        hpc_s$Global_intensity <- as.numeric(hpc_s$Global_intensity)
        hpc_s$Sub_metering_1 <- as.numeric(hpc_s$Sub_metering_1)
        hpc_s$Sub_metering_2 <- as.numeric(hpc_s$Sub_metering_2)
        hpc_s$Sub_metering_3 <- as.numeric(hpc_s$Sub_metering_3)
        hpc_s
}

createPlot <- function(hpc_s) {
        ## the default width and height is what we want
        png("plot2.png")
        plot(hpc_s$DateTime, hpc_s$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
        dev.off()
}

plot2Creator <- function() {
        ## Get the data, assuming data file is in current directory
        hpc_ds <- readDataFile("household_power_consumption.txt")
        ## create the required subset
        hpc_ds_s <- extractSubSet(hpc_ds)
        ## create and save the graph
        createPlot(hpc_ds_s)
}