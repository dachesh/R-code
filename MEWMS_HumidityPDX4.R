require(ggplot2)
require(plyr)
require(dplyr)
require(reshape2)
require(stringr)

omega <- 0.50
L <- 1.64
S <- 0

pdx4hum <- read.csv("/Users/eshelman/code/Python/DCGS_BMS_Trends/PDX4Pod20_Humidity.csv", sep = ',', header = TRUE)
pdx4hum <- pdx4hum[complete.cases(pdx4hum),]
meanvec <- as.vector(colMeans(pdx4hum[,-1],na.rm = TRUE))

testfunc <- function(pdx4hum, meanvec) {
  y <- as.numeric(c(pdx4hum)) - meanvec
  t <- crossprod(y)
  S <- omega * t + (1-omega) * S
}

apply(pdx4hum, 1, function(pdx4hum) testfunc)


print(S)

# Melt data together to generate plots #
meltdat<-melt(pdx4hum, id=c("DateTime"))

meltdat$Podnum[grepl("Pod17", meltdat$variable, ignore.case = TRUE)]<-"Pod17"
meltdat$Podnum[grepl("Pod18", meltdat$variable, ignore.case = TRUE)]<-"Pod18"
meltdat$Podnum[grepl("Pod19", meltdat$variable, ignore.case = TRUE)]<-"Pod19"
meltdat$Podnum[grepl("Pod20", meltdat$variable, ignore.case = TRUE)]<-"Pod20"

ggplot(data=meltdat, aes(x=DateTime, y=value, color=variable)) + geom_line() +
  geom_hline(y=mean(meltdat$value), color = "blue") +
  scale_x_date(breaks="day") +
  theme(axis.text.x=element_text(angle=90)) +
  #geom_text(data=alrmdat, aes(x=max(alrmdat$date)+1,y=(mean(alrmdat$alarms) +1*mean(alrmdat$alarms))),label="Allowable") + 
  #geom_text(data=alrmdat, aes(x=max(alrmdat$date)+1,y=(mean(alrmdat$alarms) +1.5*mean(alrmdat$alarms))),label="Investigate") + 
  #geom_text(data=alrmdat, aes(x=max(alrmdat$date)+1,y=(mean(alrmdat$alarms) +2*mean(alrmdat$alarms))),label="COE") + 
  #geom_text(data=alrmdat, aes(x=max(alrmdat$date)+1,y=mean(alrmdat$alarms)),label="Centerline") + 
  labs(title=paste("Global Daily Alarms through: ", Sys.Date(), sep=""))