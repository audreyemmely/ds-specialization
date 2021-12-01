pwr <- read.table("household_power_consumption.txt",skip=1,sep=";")
names(pwr) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
subpwr <- subset(pwr,pwr$Date=="1/2/2007" | pwr$Date =="2/2/2007")

#plot 1
hist(as.numeric(as.character(subpwr$Global_active_power)),col="red",main="Global Active Power",xlab="Global Active Power(kilowatts)")
title(main="Global Active Power")


subpwr$Date <- as.Date(subpwr$Date, format="%d/%m/%Y")
subpwr$Time <- strptime(subpwr$Time, format="%H:%M:%S")
subpwr[1:1440,"Time"] <- format(subpwr[1:1440,"Time"],"2007-02-01 %H:%M:%S")
subpwr[1441:2880,"Time"] <- format(subpwr[1441:2880,"Time"],"2007-02-02 %H:%M:%S")

#plot 2
plot(subpwr$Time,as.numeric(as.character(subpwr$Global_active_power)),type="l",xlab="",ylab="Global Active Power (kilowatts)") 
title(main="Global Active Power Vs Time")

#plot 3
plot(subpwr$Time,subpwr$Sub_metering_1,type="n",xlab="",ylab="Energy sub metering")
with(subpwr,lines(Time,as.numeric(as.character(Sub_metering_1))))
with(subpwr,lines(Time,as.numeric(as.character(Sub_metering_2)),col="red"))
with(subpwr,lines(Time,as.numeric(as.character(Sub_metering_3)),col="blue"))
legend("topright", lty=1, col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
title(main="Energy sub-metering")

#plot 4
par(mfrow=c(2,2))
with(subpwr,{
  plot(subpwr$Time,as.numeric(as.character(subpwr$Global_active_power)),type="l",  xlab="",ylab="Global Active Power")  
  plot(subpwr$Time,as.numeric(as.character(subpwr$Voltage)), type="l",xlab="datetime",ylab="Voltage")
  plot(subpwr$Time,subpwr$Sub_metering_1,type="n",xlab="",ylab="Energy sub metering")
  with(subpwr,lines(Time,as.numeric(as.character(Sub_metering_1))))
  with(subpwr,lines(Time,as.numeric(as.character(Sub_metering_2)),col="red"))
  with(subpwr,lines(Time,as.numeric(as.character(Sub_metering_3)),col="blue"))
  legend("topright", lty=1, col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), cex = 0.6)
  plot(subpwr$Time,as.numeric(as.character(subpwr$Global_reactive_power)),type="l",xlab="datetime",ylab="Global_reactive_power")
})