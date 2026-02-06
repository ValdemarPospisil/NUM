AddNewtonCoef <- function(x, y, a){
  n <- length(x)
  scitani <- 0
  nasobeni <- 1
  for(i in 1:(n-1)){
    scitani <- scitani + a[i]*nasobeni
    nasobeni <- nasobeni*(x[n]-x[i])
  }
  return((y-scitani)/nasobeni)
}
NewtonPolynomial <- function(z, a, x){
  n <- length(a)
  res <- a[n]
  if(n > 1){
    for(i in (n-1):1) res <- res*(z - x[i]) + a[i]
  }
  return(res)
}


n <- 10
x <- 1:n
y <- sin(x)
plot(x, y)

a <- y[1]
for(i in 2:n) a[i] <- AddNewtonCoef(x[1:i], y[i], a)

z <- seq(x[1], x[n], 0.01) 

for(i in 2:n) lines(z, NewtonPolynomial(z, a[1:i], x[1:i]), col=i+1)




l <- function(xa, x, j){
  res <- 1
  for(i in 1:length(x)){
    if(i != j) res <- res*(xa-x[i])/(x[j]-x[i])
  }
  return(res)
}
print(l(x[1], x, 1))
print(l(x[1], x, 2))
print(l(x[1], x, 8))
Lagrange <- function(xa, x, y){
  res <- 0
  for(i in 1:length(x)) res <- res+y[i]*l(xa, x, i)
  return(res)
}
plot(x,y)
lines(z, Lagrange(z, x, y))
lines(z, y[8]*l(z, x, 8), col='red')
abline(h=0, col='blue')
