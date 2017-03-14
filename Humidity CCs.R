require(RMySQL)
require(qcc)  ###   documentation references Montgomery and his SPC book   ###
require(spc)
require(ggplot2)
require(TTR)
require(matrixStats)

##  Data for internal data stream  ##
pod18 <- read.csv("/Users/eshelman/Documents/PDX4_Humidity.csv", sep=",", stringsAsFactors=FALSE)
#pod18 <- read.csv("~/Documents/PDX4POD18_TempHumid.csv", sep=",", stringsAsFactors=FALSE) #Linux code

pod18$datetime <- substr(pod18$Id, 1, nchar(pod18$Id)-4)
pod18$datetime<-as.POSIXct(pod18$datetime, format="%Y/%m/%d %H:%M", tz="", usetz=FALSE)

intenv <- data.frame(datetime=pod18$datetime, hum=pod18$IntHumAvg)
intenv$wb <- (intenv$temp-32)/1.8
intenv$wb <- ((intenv$wb*atan(0.151977*sqrt(intenv$hum+8.313659)) + atan(intenv$wb + intenv$hum) - 
              atan(intenv$hum-1.676331) + 0.00391838*(sqrt(intenv$hum)^3)*atan(intenv$hum*0.023101) - 4.686035)+32)*1.8




##  Data for external data stream  ##
pod18trend <- read.csv("/Users/eshelman/Documents/PDX4_POD18_TREND.csv", sep=",", stringsAsFactors=FALSE)
#pod18trend <- read.csv("~/Documents/PDX4_POD18Trends.csv", sep=",", stringsAsFactors=FALSE) #Linux code
pod18trend <- subset(pod18trend, select= -Point_6)
pod18trend$Point_1[grepl("TIME", pod18trend$Point_1, ignore.case=TRUE)] <- NA
pod18trend$Point_2[grepl("TIME", pod18trend$Point_2, ignore.case=TRUE)] <- NA
pod18trend$Point_3[grepl("TIME", pod18trend$Point_3, ignore.case=TRUE)] <- NA
pod18trend$Point_4[grepl("TIME", pod18trend$Point_4, ignore.case=TRUE)] <- NA
pod18trend$Point_5[grepl("TIME", pod18trend$Point_5, ignore.case=TRUE)] <- NA
pod18trend$datetime <- as.POSIXct(paste(pod18trend$X..Date, pod18trend$Time, sep=" "), format="%m/%d/%y %H:%M:%S", usetz=FALSE)

extenv <- data.frame(datetime=pod18trend$datetime, hum=pod18trend$Point_4, temp=pod18trend$Point_5)
extenv$wb <- (extenv$temp-32)/1.8
extenv$wb <- ((extenv$wb*atan(0.151977*sqrt(extenv$hum+8.313659)) + atan(extenv$wb + extenv$hum) - 
                 atan(extenv$hum-1.676331) + 0.00391838*(sqrt(extenv$hum)^3)*atan(extenv$hum*0.023101) - 4.686035)+32)*1.8

extplot <- ggplot(data=extenv, aes(x=datetime, y=temp)) + geom_line(color="darkorange") +
            geom_line(data=intenv, aes(x=datetime, y=temp), color="darkgreen")

###   Merging indoor and outdoor streams  ###
tiedenv <- merge(intenv, extenv, by="datetime")
ggplot(data=tiedenv, aes(x=datetime, y=temp.x)) + geom_line(color="darkorange") + #.x values are internal
  geom_line(data=tiedenv, aes(x=datetime, y=temp.y), color="darkgreen")           #.y values are external
ggplot(data=tiedenv, aes(x=datetime, y=hum.x)) + geom_line(color="darkblue") +
  geom_line(data=tiedenv, aes(x=datetime, y=hum.y), color="dodgerblue3")
wbplot <- ggplot(data=tiedenv, aes(x=datetime, y=wb.x)) + geom_line(color="black") +
  geom_line(data=tiedenv, aes(x=datetime, y=wb.y), color="darkgreen")

###   Moving average for 5 minute intervals   ###
IntMvgavg <- matrix(cbind(intenv$hum[1:length(intenv$hum)-1], intenv$hum[2:length(intenv$hum)]), ncol=2)
IntMRset <- data.frame(IntMvgavg)
IntMRset <- IntMRset[complete.cases(IntMRset),]
IntMRset$range <- abs(IntMRset$X1 - IntMRset$X2)
IntMRset$diff <- IntMRset$X2 - IntMRset$X1
IntMRplot <- qcc(IntMRset$range, type="xbar.one", limits=c(0,.8), title = "Control Charts: Rate of Humidity Change\n 5 Minute Intervals - Internal DC",
                 ylab="Absolute Change in Pod Humidity (RH%)")
IntMRplot2 <- qcc(IntMRset$diff, type="xbar.one", limits=c(-0.8, .8), title = "Control Charts: Humidity Change\n 5 Minute Intervals - Internal DC",
                  ylab="Change in Pod Humidity (RH%)")

ExtMvgavg <- matrix(cbind(extenv$hum[1:(length(extenv$hum)-2)], extenv$hum[2:(length(extenv$hum)-1)], extenv$hum[3:length(extenv$hum)]), ncol=3)
ExtMRset <- data.frame(ExtMvgavg)
ExtMRset <- ExtMRset[complete.cases(ExtMRset),]
ExtMRset$range <- abs(ExtMRset$X1 - ExtMRset$X2)
ExtMRset$diff <- ExtMRset$X2 - ExtMRset$X1
ExtMRplot <- qcc(ExtMRset$range, type="xbar.one", limits=c(0,.8), title = "Control Charts: Rate of Humidity Change\n 5 Minute Intervals - External",
                 ylab="Absolute Change in Pod Humidity (RH%)")
ExtMRplot2 <- qcc(ExtMRset$diff, type="xbar.one", limits=c(-0.8, .8), title = "Control Charts: Humidity Change\n 5 Minute Intervals - External",
                  ylab="Change in Pod Humidity (RH%)")


subenv <- subset(tiedenv, select = -datetime)
wbenv <- subset(subenv, select = c(wb.x, wb.y))

mult.chart(wbenv, type="chi", alpha = 0.05)
mult.chart(wbenv, type = "t2")$covariance
mult.chart(subenv, type = "mcusum")
mult.chart(intenv, type = "mewma", lambda = 0.95)

ewma(wbenv$wb.x, lambda = 0.1, nsigmas = 3)
ewma(wbenv$wb.y, lambda = 0.1, nsigmas = 3)

###   Deseasonalizing the data to smooth the daily effects   ###
desenv <- SMA(wbenv[,"wb.x"], 144)

###   Hourly moving average calcs and control charts   ###
MvgRng <- matrix(cbind(extenv$hum[1:(length(extenv$hum)-11)], extenv$hum[2:(length(extenv$hum)-10)], extenv$hum[3:(length(extenv$hum)-9)], extenv$hum[4:(length(extenv$hum)-8)], 
                       extenv$hum[5:(length(extenv$hum)-7)], extenv$hum[6:(length(extenv$hum)-6)], extenv$hum[7:(length(extenv$hum)-5)], extenv$hum[8:(length(extenv$hum)-4)], 
                       extenv$hum[9:(length(extenv$hum)-3)], extenv$hum[10:(length(extenv$hum)-2)], extenv$hum[11:(length(extenv$hum)-1)], extenv$hum[12:length(extenv$hum)]), ncol=12)  

MvgRngSet <- rowRanges(MvgRng, na.rm=TRUE)
MvgRngSet <- data.frame(MvgRngSet)
MvgRngSet$range <- MvgRngSet$X2 - MvgRngSet$X1
MRPlot <- qcc(MvgRngSet$range, type="xbar.one", limits=c(0, 10), title = "Control Chart: Rate of RH(%) Change \nover Moving 1 Hour Range - External", ylab="Absolute Change in RH(%)")

MvgRng2 <- matrix(cbind(intenv$hum[1:(length(intenv$hum)-11)], intenv$hum[2:(length(intenv$hum)-10)], intenv$hum[3:(length(intenv$hum)-9)], intenv$hum[4:(length(intenv$hum)-8)], 
                       intenv$hum[5:(length(intenv$hum)-7)], intenv$hum[6:(length(intenv$hum)-6)], intenv$hum[7:(length(intenv$hum)-5)], intenv$hum[8:(length(intenv$hum)-4)], 
                       intenv$hum[9:(length(intenv$hum)-3)], intenv$hum[10:(length(intenv$hum)-2)], intenv$hum[11:(length(intenv$hum)-1)], intenv$hum[12:length(intenv$hum)]), ncol=12)  

MvgRngSet2 <- rowRanges(MvgRng2, na.rm=TRUE)
MvgRngSet2 <- data.frame(MvgRngSet2)
MvgRngSet2$range <- MvgRngSet2$X2 - MvgRngSet2$X1
MRPlot2 <- qcc(MvgRngSet2$range, type="xbar.one", limits=c(0, 10), title = "Control Chart: Rate of RH(%) Change \nover Moving 1 Hour Range - Internal", ylab="Absolute Change in RH(%)")
