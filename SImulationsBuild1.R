require(ggplot2)

Days<-1
Hours<-Days*24
Minutes<-Days*1440

### List of MTBF data in hours
Utility<-157788
Catcher<-7000
Generator_des<-7000
Generator_gas<-5000

### Catcher failure probability distribution creation
runs<-100000
normarr<-matrix(1,nrow=runs)
unifarr<-matrix(1,nrow=runs)
exparr<-matrix(1,nrow=runs)
weibarr4<-matrix(1,nrow=runs)
weibarr5<-matrix(1,nrow=runs)
weibarr6<-matrix(1,nrow=runs)
weibarr7<-matrix(1,nrow=runs)
poiarr<-matrix(1,nrow=runs)
normavg<-matrix(1,nrow=runs)
unifavg<-matrix(1,nrow=runs)
expavg<-matrix(1,nrow=runs)
weibavg4<-matrix(1,nrow=runs)
weibavg5<-matrix(1,nrow=runs)
weibavg6<-matrix(1,nrow=runs)
weibavg7<-matrix(1,nrow=runs)
poiavg<-matrix(1,nrow=runs)
#timeset<-matrix(1,nrow=Minutes)

for (i in 1:runs){
  x<-rnorm(Minutes,mean=Catcher*60,sd=Catcher*20)
  normarr[i]<-length(x[x<Minutes])
  normavg[i]<-mean(x)
  
  x2<-runif(Minutes,min=1,max=120*Catcher)
  unifarr[i]<-length(x2[x2<=Minutes])
  unifavg[i]<-mean(x2)
  
  x3<-rexp(Minutes,rate=1/(Catcher*60))
  exparr[i]<-length(x3[x3<=Minutes])
  expavg[i]<-mean(x3)
  
  x4<-rweibull(Minutes, shape = 1, scale = Catcher*60)
  weibarr4[i]<-length(x4[x4<=Minutes])
  weibavg4[i]<-mean(x4)
  
  x5<-rweibull(Minutes, shape = 2, scale = Catcher*60)
  weibarr5[i]<-length(x5[x5<=Minutes])
  weibavg5[i]<-mean(x5)
  
  x6<-rweibull(Minutes, shape = 3, scale = Catcher*60)
  weibarr6[i]<-length(x6[x6<=Minutes])
  weibavg6[i]<-mean(x6)
  
  x7<-rweibull(Minutes, shape = 1.5, scale = Catcher*60)
  weibarr7[i]<-length(x7[x7<=Minutes])
  weibavg7[i]<-mean(x7)
  
  x8<-rpois(Minutes, lambda = Catcher*60)
  poiarr[i]<-length(x8[x8<=Minutes])
  poiavg[i]<-mean(x8)
  
}


Catch_agg<-data.frame(rbind(normarr,unifarr,exparr,weibarr4,weibarr5,weibarr6,weibarr7,poiarr, deparse.level = 2))
#Catch_agg$avg<-rowMeans(Catch_agg)
#Catch_agg$agg<-rowSums(Catch_agg)
#hist(Catch_agg$avg)
agg_table<-as.data.frame(table(Catch_agg))
agg_table$Rel_Freq<-agg_table$Freq/800000
barplot(agg_table$Rel_Freq, main="Failure Distribution - Aggregated", xlab="Failure Event Count", names.arg = agg_table$Catch_agg)



Catch_avg<-rbind(normavg,unifavg,expavg,weibavg4,weibavg5,weibavg6,weibavg7,poiavg, deparse.level = 1)
Catch_avg$avg<-rowMeans(Catch_avg)
hist(Catch_avg$avg)

plot(x)
plot(normarr)
mean(normarr)
mean(unifarr)
mean(exparr)
median(normarr)
median(unifarr)
median(exparr)

hist(Catch_agg$avg, breaks=100)
hist(Catch_avg$avg, breaks=100)

mode(normarr)
ggplot(data.frame(normarr), aes(normarr))+
  geom_histogram(aes(y=..count..))
ggplot(data.frame(unifarr), aes(unifarr))+
  geom_histogram(aes(y=..count..))
ggplot(data.frame(exparr), aes(exparr))+
  geom_histogram(aes(y=..count..))

#mean(c(normarr,unifarr,exparr))
