f <- function(t, X){
  # X = c(x[i], x[2]) <- c(x, v)
  
  return(c(X[2], -X[1]-0.1*X[2]))
}

Euler <- function(x, y, f, h){return(y+h*f(x,y))}
RK2 <- function(x, y, f, h){
  hhalf <- 0.5*h
  return(y+h*f(x+hhalf,y+h*f(x+hhalf,y+hhalf*f(x,y))))
  }

RK4 <- function(x, y, f, h){
  hhalf <- 0.5*h
  k1 <- f(x,y)
  k2 <- f(x+hhalf, y+hhalf*k1)
  k3 <- f(x+hhalf, y+hhalf*k2)
  k4 <- f(x+h, y+h*k3)
  return(y+h*(k1*2*(k2+k3)+k4)/6)
}



tMAX <- 100
n <- 10000
h <- tMAX/n
t <- seq(0, tMAX, h)
n <- n+1
X <- matrix(0, nrow = n, ncol = 2)
X[1,1] <- 100
X[1,2] <- 0
for(i in 2:n) X[i,] <- Euler(t, X[i-1,], f, h)
plot(t, X[,1], col='red', type ='l')


Xrk2 <- X
for(i in 2:n) Xrk2[i,] <- RK2(t, Xrk2[i-1,], f, h)
lines(t, Xrk2[,1], col='blue', type ='l')


Xrk4 <- X
for(i in 2:n) Xrk4[i,] <- RK4(t, Xrk4[i-1,], f, h)
lines(t, Xrk4[,1], col='violet', type ='l')



lines(t,100*exp(-0.1*t/2)*cos(t), X[,1], col='green', type ='l', add=TRUE)

