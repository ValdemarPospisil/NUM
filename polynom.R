AddNewtonCoef <- function(x, y, a){
  n <- length(x)
  scitani <- 0
  nasobeni <- 1
  for(i in 1:(n-1)){
    scitani <- scitani + a[i] * nasobeni
    nasobeni <- nasobeni * (x[n] - x[i])
  }
  return((y[n] - scitani) / nasobeni)
}

NewtonPolynomial <- function(z, a, x) {
  n <- length(a)
  res <- rep(a[n], length(z))  # Initialize result as a vector
  if(n > 1){
    for(i in (n-1):1) {
      res <- res * (z - x[i]) + a[i]
    }
  }
  return(res)
}


n <- 5
x <- 1:n
y <- sin(x)
plot(x, y, col='red')

a <- numeric(n)
a[1] <- y[1]
for(i in 2:n) {
  a[i] <- AddNewtonCoef(x[1:i], y[1:i], a)
}

z <- seq(x[1], x[n], 0.01)

for(i in 1:n) {
  lines(z, NewtonPolynomial(z, a[1:i], x[1:i]), col=i+1)
}







l <- function(xa, x, j) {
  res <- 1
  for(i in 1:length(x)) {
    if(i != j) {
      res <- res * ((xa - x[i]) / (x[j] - x[i]))
    }
  }
  return(res)
}


lagrange <- function(xa, x, y) {
  res <- 0
  for(i in 1:length(x)) {
    res <- res + y[i] * l(xa, x, i)
  }
  return(res)
}

# Sample data points
n <- 9
x <- 1:n
y <- sin(x)

# Set up plot
plot(x, y, col='red')

# Evaluate Lagrange polynomial on finer grid
z <- seq(x[1], x[n], 0.01)
lz <- sapply(z, function(z_val) lagrange(z_val, x, y))

# Plot the Lagrange polynomial
lines(z, lz, col='blue')

# Plot one of the Lagrange basis polynomials (for j = 2)
l_basis <- sapply(z, function(z_val) l(z_val, x, 2))
lines(z, y[2] * l_basis, col='red')

