ChebyshevCoef <- function(n) {
  a0 <- numeric(n) # Initialize coefficients for T0(x)
  a0[1] <- 1
  a1 <- numeric(n) # Initialize coefficients for T1(x)
  a1[2] <- 1
  for (i in 3:n) {
    a <- 2 * c(0, a1[-n]) - a0 # Generate coefficients for Tn(x)
    a0 <- a1
    a1 <- a
  }
  return(a1)
}

Newton <- function(f, fd, x0, tol = 1e-8) {
  x <- x0
  repeat {
    dx <- f(x) / fd(x) # Fix: Correct Newton's update formula
    x <- x - dx
    if (abs(dx) < tol) return(x)
  }
}

NewtonHorner <- function(a, x0, tol = 1e-8) {
  x <- x0
  repeat {
    res <- Horner(a, x)
    dx <- res[1] / res[2] # Fix: Correct the derivative-based update
    x <- x - dx
    if (abs(dx) < tol) return(x)
  }
}

Horner <- function(a, x) {
  n <- length(a)
  y <- a[n]
  yd <- 0
  if (n > 1) {
    for (i in (n - 1):1) {
      yd <- yd * x + y
      y <- y * x + a[i]
    }
  }
  return(c(y, yd))
}

PolX <- function(a, x) {
  n <- length(a)
  y <- 0
  yd <- 0
  for (i in 1:n) {
    y <- y + a[i] * x^(i - 1)
    if (i > 1) {
      yd <- yd + (i - 1) * a[i] * x^(i - 2)
    }
  }
  return(c(y, yd))
}

# Example 1: Use Horner and PolX
n <- 2
x <- 2
a <- runif(n)
cat("Horner result:", Horner(a, x), "\n")
cat("PolX result:", PolX(a, x), "\n")

# Example 2: Plotting cos(x) and intersection with y = x
x <- seq(0, 1, 0.0001)
plot(x, cos(x), ylim = c(0, 1), type = 'l', col = 'red', main = "Intersection of cos(x) and y=x")
abline(a = 0, b = 1, col = 'black')
lines(x, x, col = 'green')

# Intersection using Newton's method
inters <- Newton(f = function(x) cos(x) - x, fd = function(x) -sin(x) - 1, x0 = 0, tol = 1e-8)
points(inters, cos(inters), col = 'blue', cex = 2)
cat("Intersection at:", inters, "\n")

# Example 3: Chebyshev Polynomials
a <- ChebyshevCoef(7)
x <- seq(-1, 1, 0.0001)
chebyshev_values <- sapply(x, function(x) Horner(a, x)[1])
plot(x, chebyshev_values, type = 'l', col = 'red', main = "Chebyshev Polynomial T4(x)", ylab = "T4(x)", xlab = "x")
