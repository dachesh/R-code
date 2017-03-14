require("qcc")
require("plyr")
require("reshape2")
require("grid")
require("ggplot2")

tempdat<-read.delim("/users/eshelman/Documents/IAD15_5Min_Temps.csv", header=TRUE, sep=",")
humdat<-read.delim("/users/eshelman/Documents/IAD15_5Min_Humidity.csv", header=TRUE, sep=",")
tempdat$Label<-as.POSIXct(tempdat$Label, "%Y/%m/%d %H:%M", tz="America/Los_Angeles")
humdat$Label<-as.POSIXct(humdat$Label, "%Y/%m/%d %H:%M", tz="America/Los_Angeles")

tempdat <- melt(tempdat, id='Label')
humdat <- melt(humdat, id='Label')

tempdat$room[grepl("Pod_1", tempdat$variable, ignore.case=TRUE)]<-"Pod_1"
tempdat$room[grepl("Pod_2", tempdat$variable, ignore.case=TRUE)]<-"Pod_2"
tempdat$room[grepl("Pod_3", tempdat$variable, ignore.case=TRUE)]<-"Pod_3"
tempdat$room[grepl("Pod_4", tempdat$variable, ignore.case=TRUE)]<-"Pod_4"
tempdat$room[grepl("Pod_5", tempdat$variable, ignore.case=TRUE)]<-"Pod_5"
tempdat$room[grepl("Pod_6", tempdat$variable, ignore.case=TRUE)]<-"Pod_6"

humdat$room[grepl("Pod_1", humdat$variable, ignore.case=TRUE)]<-"Pod_1"
humdat$room[grepl("Pod_2", humdat$variable, ignore.case=TRUE)]<-"Pod_2"
humdat$room[grepl("Pod_3", humdat$variable, ignore.case=TRUE)]<-"Pod_3"
humdat$room[grepl("Pod_4", humdat$variable, ignore.case=TRUE)]<-"Pod_4"
humdat$room[grepl("Pod_5", humdat$variable, ignore.case=TRUE)]<-"Pod_5"
humdat$room[grepl("Pod_6", humdat$variable, ignore.case=TRUE)]<-"Pod_6"

tempset<-aggregate(tempdat$value, by=list(tempdat$room, tempdat$Label), FUN=mean, na.rm=TRUE)
names(tempset)[names(tempset)=="Group.1"] <- "Room"
names(tempset)[names(tempset)=="Group.2"] <- "Time"
names(tempset)[names(tempset)=="x"] <- "Temp"

ggplot(data = tempset, aes(x=tempset$Time, y=tempset$Temp, color=tempset$Room)) + geom_line()

humset<-aggregate(humdat$value, by=list(humdat$room, humdat$Label), FUN=mean, na.rm=TRUE)
names(humset)[names(humset)=="Group.1"] <- "Room"
names(humset)[names(humset)=="Group.2"] <- "Time"
names(humset)[names(humset)=="x"] <- "Humidity"

ggplot(data = humset, aes(x=humset$Time, y=humset$Humidity, color=humset$Room)) + geom_line()

wetbulb<-merge(tempset, humset, by=c("Room", "Time"), na.rm=TRUE)
wetbulb$Temp<-(wetbulb$Temp-32)/1.8

wetbulb$wbtemp<-wetbulb$Temp*atan(0.151977*(sqrt(wetbulb$Humidity+8.313659)))+atan(wetbulb$Temp+wetbulb$Humidity)-atan(wetbulb$Humidity-1.676331)+
                0.00391838*(wetbulb$Humidity^(3/2))*atan(0.023101*wetbulb$Humidity)-4.686035

#wetbulb<-wetbulb[complete.cases(wetbulb),]

ggplot(data = wetbulb, aes(x=wetbulb$Time, y=wetbulb$wbtemp, color=wetbulb$Room)) + geom_line()

#sensor<-unique(plotdat$variable)
rmlst <- unique(wetbulb$Room)



for (i in unique(wetbulb$Room)) {
  print(i)
  j <- gsub("\\s","",paste(i))
  datset<-subset(wetbulb, Room==i)
  t <- ggplot(data = datset, aes(x=datset$Time, y=datset$Temp)) + geom_line() +
        geom_hline(aes(yintercept=mean(datset$Temp, na.rm=TRUE), color="darkorange")) +
        geom_hline(aes(yintercept=35), color="darkred") +
        geom_hline(aes(yintercept=30), color="darkgreen") +
        geom_hline(aes(yintercept=25), color="darkblue") +
        scale_x_datetime(breaks="day") +
        xlab("Date") + ylab("Room Temp (C)") +
        theme(axis.text.x=element_text(angle=90))
  
  h <- ggplot(data = datset, aes(x=datset$Time, y=datset$Humidity)) + geom_line() +
        geom_hline(aes(yintercept=mean(datset$Humidity, na.rm=TRUE), color="darkorange")) +
        geom_hline(aes(yintercept=75), color="darkred") +
        geom_hline(aes(yintercept=25), color="darkblue") +
        scale_x_datetime(breaks="day") +
        xlab("Date") + ylab("Humidity Levels (RH%)") +
        theme(axis.text.x=element_text(angle=90))
      
  g <- ggplot(data = datset, aes(x=datset$Time, y=datset$wbtemp)) + geom_line() +
        geom_hline(aes(yintercept=mean(datset$wbtemp, na.rm=TRUE), color="darkorange")) +
        geom_hline(aes(yintercept=35), color="darkred") +
        geom_hline(aes(yintercept=33), color="darkgreen") +
        geom_hline(aes(yintercept=28.33), color="darkblue") +
        scale_x_datetime(breaks="day") +
        xlab("Date") + ylab("Wet Bulb Temp (C)") +
        theme(axis.text.x=element_text(angle=90))
  
  
  png(file = paste("~/Documents/ReportCharts/Sensor", j, "Temp.png",sep=""),width=1080, height=480, res=120)
  plot(t, chart.all=TRUE)
  dev.off()
  
  png(file = paste("~/Documents/ReportCharts/Sensor", j, "Humid.png",sep=""),width=1080, height=480, res=120)
  plot(h, chart.all=TRUE)
  dev.off()
  
  png(file = paste("~/Documents/ReportCharts/Sensor", j, "wtblb.png",sep=""),width=1080, height=480, res=120)
  plot(g, chart.all=TRUE)
  dev.off()
  
}

############## Experimental develpment of Temperature Control Charts ###############
wbRm1<- subset(wetbulb, Room == "Pod_1")
wbRm1<-wbRm1[complete.cases(wbRm1),]
#wbRm1<-wbRm1$wbtemp
Rmsd<-sd(wbRm1$wbtemp, na.rm=TRUE)
Minval <- as.integer(min(wbRm1$wbtemp, na.rm=TRUE))
Maxval <- as.integer(max(wbRm1$wbtemp, na.rm=TRUE))
#x <- qcc(wbRm1, type = "xbar.one", title = "Individuals Chart\nfor Temperature sample data", ylim = c(0,40))
#x <- qcc(matrix(cbind(wbRm1[1:length(wbRm1)-1], wbRm1[2:length(wbRm1)]), ncol = 2), type = "R", title = "Moving Range Chart\nfor Temperature sample data", ylim = c(0,40))

###   Individual observation plot   ###
ggplot(data=wbRm1, aes(x=wbRm1$Time, y=wbRm1$wbtemp)) + geom_line() +
  geom_hline(aes(yintercept=(mean(wbRm1$wbtemp, na.rm=TRUE) + 3*Rmsd)), color="darkred") +
  geom_hline(aes(yintercept=(mean(wbRm1$wbtemp, na.rm=TRUE) - 3*Rmsd)), color="blue") +
  geom_hline(aes(yintercept=(mean(wbRm1$wbtemp, na.rm=TRUE))), color="black", linetype = 2) +
  scale_x_datetime(breaks="day") +
  scale_y_discrete() +
  xlab("Date") + ylab("Room Temp (C)") +
  theme(axis.text.x=element_text(angle=90))

  
###   Moving Range plot - Five minute intervals   ###  This does work, but may not be important!
mrRm1 <- matrix(cbind(wbRm1$wbtemp[1:length(wbRm1$wbtemp)-1], wbRm1$wbtemp[2:length(wbRm1$wbtemp)]), ncol=2)
x<-qcc(mrRm1, type="R")


###   EWMA - Really? Why bother with this?   ###
ewma(wbRm1$wbtemp, sizes = 1, lambda = 0.1, nsigmas = 2.5, xlab="Sample Number",
     ylab="Weighted Moving Average of Temperature - 5 Min. Interval", 
     title="EWMA chart for Wet Bulb Temperature (C)", plot = TRUE, add.stats=TRUE, 
     chart.all=TRUE, axis.las=0)