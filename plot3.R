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
        png("plot3.png")
        with(hpc_s, plot(DateTime, Sub_metering_1, main = "", type="l", xlab="", ylab="Energy sub metering"))
        with(hpc_s, lines(DateTime, Sub_metering_2, col="red"))
        with(hpc_s, lines(DateTime, Sub_metering_3, col="blue"))
        legend("topright", col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), seg.len=3, lty="solid")
        dev.off()
}

plot3Creator <- function() {
        ## Get the data, assuming data file is in current directory
        hpc_ds <- readDataFile("household_power_consumption.txt")
        ## create the required subset
        hpc_ds_s <- extractSubSet(hpc_ds)
        ## create and save the graph
        createPlot(hpc_ds_s)
}