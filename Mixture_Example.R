library(mclust)
library(mvtnorm)
nn <- 10000
sep <- 2

data <- cbind(rnorm(nn,sep*ceiling(runif(nn)*3)),rnorm(nn,sep*ceiling(runif(nn)*2)))
# data <- cbind(rt(nn,5,sep*ceiling(runif(nn)*3)),rt(nn,5,sep*ceiling(runif(nn)*2)))


CLUSTOUT <- Mclust(data,G=1:8,modelNames = 'VVV')

LOGLIK <- as.numeric((CLUSTOUT$BIC+log(nn)*(1:8*2+1:8*3-1))/2)

nu <- 1000
const <- log(nu)/sqrt((log(log(nu)))*nu)/2
BIC <- -LOGLIK/nn+log(nn)/nn*(1:8*2+1:8*3)/2
which.min(BIC)
SWIC <-  -LOGLIK/nn+const*sqrt(log(log(nn)))/sqrt(nn)*(1:8*2+1:8*3)
which.min(SWIC)

plot(data)

for (sep in c(2,3)) {
  for (cols in 2:3) {
    for (nn in c(100,1000,10000,100000)) {
      STOREMAT <- matrix(NA,100,8)
      for (ii in 1:100) {
        data <- cbind(rnorm(nn,sep*ceiling(runif(nn)*cols)),rnorm(nn,sep*ceiling(runif(nn)*2)))
        CLUSTOUT <- Mclust(data,G=1:8,modelNames = 'VVV')
        LOGLIK <- as.numeric((CLUSTOUT$BIC+log(nn)*(1:8*2+1:8*3-1))/2)  
        STOREMAT[ii,] <- LOGLIK
        print(c(sep,cols,nn,ii))
      }
      save(STOREMAT,file=paste('sep_',sep,'_cols_',cols,'_nn_',nn,'.rdata',sep=''))
    }
  }
}