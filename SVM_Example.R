library(compiler)
enableJIT(3)

for (numb in c(100,1000,10000,100000)) {
  for (epsi in c(0,1)) {
    loss <- function(x) {
      max(0,abs(x)-epsi)
    }
    vecloss <- Vectorize(loss)
    for (tt in c(4,6)) {
        STOREMAT <- matrix(NA,100,10)
        for (ii in 1:100) {
          nn <- numb
          xx <- runif(nn*10)
          Xmat <- matrix(xx,nn,10)
          true <- tt
          if (true == 4) {
            Y <- c(c(seq(from=1,by=-0.25,length.out=true),rep(0,10-true)) %*% t(Xmat)) + rnorm(nn,0,1)        
          }
          if (true == 6) {
            Y <- c(c(seq(from=1,by=-0.15,length.out=true),rep(0,10-true)) %*% t(Xmat)) + rnorm(nn,0,1)
          }
          for (jj in 1:10) {
            order <- jj
            Xord <- Xmat[,1:order]
            object <- function(para) {
              mean(vecloss(Y-c(para %*% t(Xord))))
            }
            
            opti <- optim(rep(0,order),fn=object,method='BFGS',control = list(maxit = 100, abstol = 10e-5, reltol=10e-5))
            # opti
            STOREMAT[ii,jj] <- opti$value
            print(c(ii,jj))
          }
          print(STOREMAT[ii,])
        }
        save(STOREMAT,file=paste('SVM_epsi_',epsi,'_tt_',tt,'_numb_',numb,'.rdata',sep=''))
    } 
  }
}
