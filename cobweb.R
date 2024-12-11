# Define the function
g <- function(x) {
  return(sqrt(x + 10) / 2)
}

# -----------------------------------------------
xstart <- 5
plot(g, xlim = c(0, xstart), ylim = c(0, xstart), col = 'blue', main = "Cobweb Diagram")
abline(a = 0, b = 1) # y = x line

n <- 40
x <- numeric(n)
x[1] <- 5

for (i in 2:n) {
  x[i] <- g(x[i - 1]) # Update the value
  segments(x0 = x[i - 1], y0 = x[i - 1], x1 = x[i - 1], y1 = x[i], col = 'green') # Vertical line
  segments(x0 = x[i - 1], y0 = x[i], x1 = x[i], y1 = x[i], col = 'violet') # Horizontal line
}

#plot(x, type = 'b', col = 'red', main = "Iterations Over Time", xlab = "Iteration", ylab = "Value")
# ---------------------------------------


# Plot the function
xstart <- 5
plot(cos, xlim = c(0, xstart), ylim = c(0, xstart), col = 'blue', main = "Cobweb Diagram")
abline(a = 0, b = 1) # y = x line

# Parameters for iteration
n <- 40
x <- numeric(n) # 'b' was incorrect; use 'n' for the size of the numeric vector
x[1] <- 5 # Initial value

# Iterative cobweb diagram generation
for (i in 2:n) {
  x[i] <- cos(x[i - 1]) # Update the value
  # Draw segments for the cobweb diagram
  segments(x0 = x[i - 1], y0 = x[i - 1], x1 = x[i - 1], y1 = x[i], col = 'green') # Vertical line
  segments(x0 = x[i - 1], y0 = x[i], x1 = x[i], y1 = x[i], col = 'violet') # Horizontal line
}

# Plot the iterations as a time series
#plot(x, type = 'b', col = 'red', main = "Iterations Over Time", xlab = "Iteration", ylab = "Value")
# ---------------------------------------
# g <- function(y1) {
#  return(y0 + hpul *(f(x1,y1) + f(x0,y0)))
# }

step <- function(f, h, x0, y0){
  hpul <- 0.5*h
  x1 <- x0+h
  y1 <- y0 + h * f(x0, y0)
  for (i in 1:10){
    y1 <- y0 + hpul *(f(x1,y1) + f(x0,y0))
  }
  return(y1)
}

f <- function(x,y) {return((1+y*y)/(x*y*(1+x*x)))}

h <- 0.01
x <- seq(1, 2, h)
n <- length(x)
y <- numeric(n)
y[1] <- 1
for(i in 2:n) y[i] <- step(f, h, x[i-1], y[i-1])
plot(x,y, col='blue')
lines(x, (sqrt(3*x*x-1))/(1+x*x))

# -----------------------------------------

f <- function(x,y) {
  beta <- 1
  nu <- 1
  dS <- beta *y[1]*y[2]
  dI <- nu * y[2]
  
  return(c(-dS, dS - dI, dI))
}

h <- 0.1
x <- seq(0, 10, h)
n <- length(x)
y <- matrix(0, nrow = n, ncol=3)
y[1, ] <- c(999, 1, 0)


for(i in 2:n) y[i, ] <- step(f, h, x[i-1], y[i-1, ])
plot(x,y, col='blue')
points(x, )
points(x, y[,2], col='red')
points(x, y[,3], col='green')


