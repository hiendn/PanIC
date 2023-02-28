library(mvtnorm)
library(pracma)

for (nnset in c(100,1000,10000,100000)) {
  for (kset in c(4,6)) {
    for (Hardness in 0:1) {
      STOREMAT <- matrix(NA,100,10)
      for (ii in 1:100) {
        nn <- nnset
        kk0 <- kset
        Hard <- Hardness
        Y <- rmvnorm(nn,sigma = diag(0.25,kk0)+matrix(0.75,kk0,kk0))
        # Y <- rmvt(nn,sigma = diag(0.25,kk0)+matrix(0.75,kk0,kk0),df=5)
        
        if (Hard == 0) {
          if (kk0 == 4) {
            ORTHO <- matrix(c(
              1,0,0,0,
              0,0.25,0,0,
              0,0,.1,0,
              0,0,0,.1,
              1,0.25,0,0,
              0,0.25,.1,0,
              0,0,.5,.1,
              1,0.25,.1,0,
              0,0.25,.1,.1,
              1,0.25,.1,.1
            ),10,4,byrow = TRUE)  
          }
          if (kk0 == 6) {
            ORTHO <- matrix(c(
              1,0,0,0,0,0,
              0,.25,0,0,0,0,
              0,0,.25,0,0,0,
              0,0,0,.1,0,0,
              0,0,0,0,.1,0,
              0,0,0,0,0,.1,
              1,.25,0,0,0,0,
              0,.25,.25,0,0,0,
              0,0,.25,.1,0,0,
              0,0,0,.1,.1,0
            ),10,6,byrow=TRUE)
          }
        }
        if (Hard == 1) {
          if (kk0 == 4) {
            ORTHO <- matrix(c(
              1,0,0,0,
              0,.5,0,0,
              0,0,.25,0,
              0,0,0,.1,
              1,.5,0,0,
              0,.5,.25,0,
              0,0,.25,.1,
              1,.5,.25,0,
              0,.5,.25,.1,
              1,.5,.25,.1
            ),10,4,byrow = TRUE)  
          }
          if (kk0 == 6) {
            ORTHO <- matrix(c(
              1,0,0,0,0,0,
              0,.5,0,0,0,0,
              0,0,.25,0,0,0,
              0,0,0,.25,0,0,
              0,0,0,0,.1,0,
              0,0,0,0,0,.1,
              1,.5,0,0,0,0,
              0,.5,.25,0,0,0,
              0,0,.25,.25,0,0,
              0,0,0,.25,.1,0
            ),10,6,byrow=TRUE)
          }
        }
        X <- Y%*%t(ORTHO)
        
        PC <- prcomp(X,center=FALSE)[[2]]
        for (jj in 1:10) {
          LOADS <- PC[,1:jj]
          STOREMAT[ii,jj] <- norm(X-X%*%LOADS%*%t(LOADS),type='F')^2/nn  
        }
        print(c(Hardness,kset,nnset,ii,STOREMAT[ii,]))
      }
      save(STOREMAT,file=paste('PCA_hardness_',Hardness,'_kk0_',kset,'_nn_',nnset,'.rdata',sep=''))
    }
  }
}
