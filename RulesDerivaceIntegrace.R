expTalyor <- function(x, n=1){
  term <- rep(1, length(x))
  res <- 1
  if(n > 1){
    term <- term*x/(i-1)
    res <- res + term
  }
  return(res)
}

a <- -3
b <- 3
x <- seq(a, b, 0.001)
plot(exp, xlim = c(a,b), lwd=2)
for(i in 1:8){
  lines(x, expTalyor(x, i), col=i+1+1)
}
#s---------------------------------

FirstDerA <- function(f, x, h){return ((f(x+h)-f(x))/h)}

FirstDerB <- function(f, x, h){return ((f(x+h)-f(x-h))/(2*h)}

SecondDer <- function(f, x, h){return ((f(x+h)-2*f(x-h))/(h*h)}


a <- 0
b <- 2*pi
plot(cos, xlim = c(a,b), lwd=2)
h <- 0.000000005
plot(function(x) FirstDerA(sin, x, h), xlim = c(a,b), lwd=2,col=2, add=TRUE)

plot(function(x) FirstDerA(sin, x, h)-cos(x),xlim = c(a,b), lwd=2)
h <- 0.0000000005
plot(function(x) FirstDerB(sin, x, h)-cos(x),xlim = c(a,b), lwd=2, col='red', add=TRUE)

plot(function(x) FirstDerA(sin, x, h)-cos(x),xlim = c(a,b), lwd=2, col='gray', add=TRUE)




a <- 0
b <- 2*pi
plot(function(x) -sin(x), xlim = c(a,b), lwd=2)
h <- 0.00005
plot(function(x) SecondDer(sin, x, h), xlim = c(a,b), lwd=2,col='red', add=TRUE)


# ------------------------------------------
# Midpoint Rule

MidPointRule <- function(f, a, b, n=1){
  h <- (b-a)/n
  return(h*sum(f(h*(1:n)+a-h*0.5)))
  
}  


a <- 0
b <- pi
print(cos(a)-cos(b))

cat("Exact solution int_a^b sin(x): ", cos(a)-cos(b), "\n")
for(i in 0:10) cat("Midpoint rule for n: ", MidPointRule(sin, a , b, 2^i), "\n")

# -----------------------------------
# Simpsons rule

SimpsonRule <- function(f, a, b, n=1){
  h <- (b-a)/n
  hpul <- h/2
  suma <- f(a) + f(b)
  xs <- h*(1:n) + a - hpul
  suma <- suma + sum(f(xs))
  if(n > 1){
    xl <- (xs + hpul)[-n]
    suma <- suma + 2*sum(f(xl))
  }  
  return(suma*h/6)
}


for(i in 0:20) cat("Simpson rule for n: ", SimpsonRule(sin, a , b, 2^i), "\n")

