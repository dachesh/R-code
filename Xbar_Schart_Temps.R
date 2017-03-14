require("qcc")
require("plyr")
require("reshape2")
require("grid")
require("ggplot2")

tempdat<-read.delim("~/Documents/DataFiles/IAD15_5Min_Temps.csv", header=TRUE, sep="\t")
humdat<-read.delim("~/Documents/DataFiles/IAD15_5Min_RH.csv", header=TRUE, sep="\t")
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

### X-bar and R Charts for humidity sets ###
##### Initial code set for one pod ######
humsub <- subset(humdat, room == "Pod_3")
humsub <- humsub[complete.cases(humsub),]
humidity<-qcc.groups(humsub$value, humsub$Label)
obj<-qcc(humidity[1:500,1], type="xbar")
  qcc(data= humidity, type="R", nsigmas=3)
  qcc(data= humidity, type="xbar", nsigmas=3)
humidR <- stats.R(humsub$value, sizes=3)



wetbulb<-merge(tempset, humset, by=c("Room", "Time"), na.rm=TRUE)
wetbulb$Temp<-(wetbulb$Temp-32)/1.8

wetbulb$wbtemp<-wetbulb$Temp*atan(0.151977*(sqrt(wetbulb$Humidity+8.313659)))+atan(wetbulb$Temp+wetbulb$Humidity)-atan(wetbulb$Humidity-1.676331)+
                0.00391838*(wetbulb$Humidity^(3/2))*atan(0.023101*wetbulb$Humidity)-4.686035

wetbulb<-wetbulb[complete.cases(wetbulb),]

ggplot(data = wetbulb, aes(x=wetbulb$Time, y=wetbulb$wbtemp, color=wetbulb$Room)) + geom_line()

### X-bar and R Charts for wetbulb sets ###
##### Initial code set for one pod ######
wbsub <- subset(wetbulb, Room == "Pod_3")
wbsub <- wbsub[complete.cases(wbsub),]
wbset<-qcc.groups(wbsub$wbtemp, wbsub$Time)
  qcc(data= wbset, type="R", nsigmas=3)
  qcc(data= wbset, type="xbar", nsigmas=3)




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