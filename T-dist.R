x<-seq(-4,4, length = 100)
hx<-dnorm(x)
degf<-c(1:30)

plot(x, hx, type="l", lty=2)

lines(x,dt(x,degf[1]),lwd=2)
lines(x,dt(x,degf[2]),lwd=2, col="blue")
lines(x,dt(x,degf[5]),lwd=2, col="darkgreen")
lines(x,dt(x,degf[10]),lwd=2, col="red")
lines(x,dt(x,degf[15]),lwd=2, col="gold")
lines(x,dt(x,degf[20]),lwd=2, col="darkorange")
lines(x,dt(x,degf[30]),lwd=2, col="black")

for (i in 1:30){
  lines(x, dt(x,degf[i]),lwd=2)
}
