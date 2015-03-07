readDataFile <- function(fileName) {
        ## reads Electric power consumption file
        ## it is big but not too big
        hpc <- read.table(fileName, header=TRUE, sep=";", stringsAsFactors=FALSE)
        hpc
}

extractSubSet <- function(hpc) {
        ## Limits to the desired two dates
        ## No ? in this subset noticed
        hpc$DateTime <- as.Date(hpc$Date, "%d/%m/%Y")
        hpc_s <- subset(hpc, DateTime == as.Date("2007-02-01", "%Y-%m-%d") | DateTime == as.Date("2007-02-02", "%Y-%m-%d"))
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
        png("plot1.png")
        hist(hpc_s$Global_active_power, col="red", xlab="Global Active Power (kilowatts)", ylab="Frequency", main="Global Active Power")
        dev.off()
}

plot1Creator <- function() {
        ## Get the data, assuming data file is in current directory
        hpc_ds <- readDataFile("household_power_consumption.txt")
        ## create the required subset
        hpc_ds_s <- extractSubSet(hpc_ds)
        ## create and save the graph
        createPlot(hpc_ds_s)
}