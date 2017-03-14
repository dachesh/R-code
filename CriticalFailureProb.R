require(ggplot2)
require(mc)
pois_set = rpois(100000,42000)
mean(pois_set)
sd(pois_set)

minutes<-60*24
minprob<-1/42000

ggplot(data.frame(pois_set), aes(pois_set))+
  geom_histogram(aes(y=..density..))


runs <- 10000
tril <- dbinom(2, minutes, minprob) #Binomial Prob of 2 failures per day (hourly)

poitri <- ppois(1, lambda = 1/1440, lower.tail = F) #Poisson Prob of 2 or more failures per day (hourly)

### Negative binomial simulation ###
negbinarr<-matrix(,nrow=runs)
for (i in 1:runs){
K=1;
p=minprob;
r=2;
success=0;
while(success<r)
{if (runif(1)>p)
{K=K+1;
  print=0 #Failure (system still works)
}else
{success=success+1;
    print=1}} #Success (system failure)
K+r-1
negbinarr[i]<-K
}
mean(negbinarr)
median(negbinarr)
mode(negbinarr)
ggplot(data.frame(negbinarr), aes(negbinarr))+
  geom_histogram(aes(y=..density..))

### Geometric simulation for first failure ###
K=1;
p=minprob;
while(runif(1)>p)
  K=K+1;

## Exponential trials ##
rsunif <- function(n) { n1 <- n+1
cE <- cumsum(rexp(n1)); cE[seq_len(n)]/cE[n1] }
plot(rsunif(1000), ylim=0:1, pch=".")
abline(0,1/(1000+1), col=adjustcolor(1, 0.5))