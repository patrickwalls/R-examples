linreg <- function(x,y) {
  
  Y <- matrix(y,nrow=length(y))
  M <- matrix(nrow=length(x),ncol=2)
  M[,1] <- 1
  M[,2] <- matrix(x,ncol=1)
  
  A <- solve( t(M) %*% M ) %*% t(M) %*% Y
  
  plot(x,y)
  xlm <- seq(min(x),max(x),length=3)
  ylm <- A[1,1] + A[2,1]*xlm
  lines(xlm,ylm,col='red')
  
  return(A)
}