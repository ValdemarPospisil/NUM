EulerStep <- function(f, x, y, h){return(y+h*f(x,y))}
EulerStep2a <- function(f, x, y, h)
{
  hpul <- h*0.5
  return(y+h*f(x+hpul,EulerStep(f, x, y, hpul)))
  }
EulerStep2b <- function(f, x, y, h)
{
  f(x, EulerStep(f, x, y, h))
  return(y+h*f(x,y))
}


f <- function(x, y){return(-y)}


N <- 10
tMax <- 2*log(2)
dt <- tMax/N
t <- dt*(0:N)
n0 <- 300

n <- t
n[1] <- n0
for(i in 1:N) n[i+1] <- EulerStep(f, t[i], n[i], dt)

plot(function(t) n0*exp(-t), col='red', lwd=2)
points(t, n, col='blue')


n2a <- t
n2a[1] <- n0
for(i in 1:N) n[i+1] <- EulerStep2a(f, t[i], n2a[i], dt)
points(t, n, col='green')


n2b <- t
n2b[1] <- n0
for(i in 1:N) n[i+1] <- EulerStep2b(f, t[i], n2b[i], dt)
points(t, n, col='cyan')