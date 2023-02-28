AIC <- c()
BIC <- c()
SWIC.1.100 <- c()
SWIC.1.1000 <- c()
SWIC.2.100 <- c()
SWIC.2.1000 <- c()

nn <- 100

comp <- 4

for (ii in 1:100) {
  
  AIC[ii] <- which.min(STOREMAT[ii,]+1/nn*((1:10)*10))
  
  BIC[ii] <- which.min(STOREMAT[ii,]+log(nn)/nn*((1:10)*10)/2)
  
  const <- log(1000)/sqrt((log(1000))*1000)/2
  SWIC.1.100[ii] <- which.min(STOREMAT[ii,]+const*sqrt(log(nn)/nn)*((1:10)*10))
  
  const <- log(10000)/sqrt((log(10000))*10000)/2
  SWIC.1.1000[ii] <- which.min(STOREMAT[ii,]+const*sqrt(log(nn)/nn)*((1:10)*10))
  
  const <- log(1000)/sqrt(log(log(1000))*1000)/2
  SWIC.2.100[ii] <- which.min(STOREMAT[ii,]+const*sqrt(log(log(nn))/nn)*((1:10)*10))
  
  const <- log(10000)/sqrt(log(log(10000))*10000)/2
  SWIC.2.1000[ii] <- which.min(STOREMAT[ii,]+const*sqrt(log(log(nn))/nn)*((1:10)*10))
}

c(mean(AIC),mean(AIC==comp))
c(mean(BIC),mean(BIC==comp))
c(mean(SWIC.1.100),mean(SWIC.1.100==comp))
c(mean(SWIC.1.1000),mean(SWIC.1.1000==comp))
c(mean(SWIC.2.100),mean(SWIC.2.100==comp))
c(mean(SWIC.2.1000),mean(SWIC.2.1000==comp))