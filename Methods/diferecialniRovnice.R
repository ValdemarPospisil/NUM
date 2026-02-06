EulerStep <- function(f, x, y, h){return(y+h*f(x,y))}
EulerStepIIa <- function(f, x, y, h){
  hpul <- h*0.5
  return(y+h*f(x+hpul,EulerStep(f, x, y, hpul)))
}
EulerStepIIb <- function(f, x, y, h){
  return(y+h*0.5*(f(x, y) + f(x, EulerStep(f, x, y, h))))
}
RK4 <- function(f, x, y, h){
  hpul <- 0.5*h
  k1 <- f(x, y)
  xshift <- x + hpul
  k2 <- f(xshift, y + hpul*k1)
  k3 <- f(xshift, y + hpul*k2)
  k4 <- f(x+h, y + h*k3)
  return(y+h*(k1+2*(k2+k3)+k4)/6)
}
f <- function(x, y){return(-y)}
fSchwingung <- function(x, y){
  return(c(y[2], -y[1]-0.1*y[2]))
}

N <- 3
tMax <- 2*log(2)
dt <- tMax/N
t <- dt*(0:N)
n0 <- 300

n <- t
n[1] <- n0
for(i in 1:N) n[i+1] <- EulerStep(f, t[i], n[i], dt)

plot(function(t) n0*exp(-t), xlim = c(0, tMax), col='red', lwd=2)
points(t, n, col='blue')

nIIa <- t
nIIa[1] <- n0
for(i in 1:N) nIIa[i+1] <- EulerStepIIa(f, t[i], nIIa[i], dt)
points(t, nIIa, col='green')

nIIb <- t
nIIb[1] <- n0
for(i in 1:N) nIIb[i+1] <- EulerStepIIb(f, t[i], nIIb[i], dt)
points(t, nIIb, col='cyan')

nRK4 <- t
nRK4[1] <- n0
for(i in 1:N) nRK4[i+1] <- RK4(f, t[i], nRK4[i], dt)
points(t, nRK4, col='violet')

# --------------------------------------
N <- 200
tMax <- 8*pi
dt <- tMax/N
t <- dt*(0:N)
x0 <- 1

x <- matrix(0, nrow=N+1, ncol=2)
x[1, 1] <- x0
x[1, 2] <- 0
for(i in 1:N) x[i+1,] <- EulerStep(fSchwingung, t[i], x[i, ], dt)

plot(t, x0*exp(-0.05*t)*cos(t), xlim = c(0, tMax), col='red', lwd=2, type='l')
points(t, x[,1], col='blue')

xIIa <- matrix(0, nrow=N+1, ncol=2)
xIIa[1, 1] <- x0
xIIa[1, 2] <- 0
for(i in 1:N) xIIa[i+1,] <- EulerStepIIa(fSchwingung, t[i], xIIa[i, ], dt)
points(t, xIIa[,1], col='green')

xIIb <- matrix(0, nrow=N+1, ncol=2)
xIIb[1, 1] <- x0
xIIb[1, 2] <- 0
for(i in 1:N) xIIb[i+1,] <- EulerStepIIb(fSchwingung, t[i], xIIb[i, ], dt)
points(t, xIIb[,1], col='cyan')

xRK4 <- matrix(0, nrow=N+1, ncol=2)
xRK4[1, 1] <- x0
xRK4[1, 2] <- 0
for(i in 1:N) xRK4[i+1,] <- RK4(fSchwingung, t[i], xRK4[i, ], dt)
points(t, xRK4[,1], col='violet')