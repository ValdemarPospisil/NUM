# Function to compute numerical derivatives using Richardson Extrapolation
fd <- function(f, x, hmax, n = 1) {
  h <- hmax / 2^(0:(n - 1))
  res <- (f(x + h) - f(x - h)) / (2 * h)
  if (n > 1) {
    for (i in 1:(n - 1)) {
      m <- n - i + 1
      power <- 4^i
      res <- (power * res[2:m] - res[1:(m - 1)]) / (power - 1)
    }
  }
  return(res)
}

# Midpoint Rule implementation
MidpointRule <- function(f, a, b, n = 1) {
  h <- (b - a) / n
  return(h * sum(f(a + h * (1:n) - h * 0.5)))
}

# Midpoint Rule with Richardson Extrapolation
MidPointRuleRichardson <- function(f, a, b, n = 1) {
  res <- sapply(2^(0:(n - 1)), function(n) MidpointRule(f, a, b, n))
  if (n > 1) {
    for (i in 1:(n - 1)) {
      m <- n - i + 1
      power <- 4^i
      res <- (power * res[2:m] - res[1:(m - 1)]) / (power - 1)
    }
  }
  return(res)
}

# Example calculations with better output
cat("Numerical Derivative Check:\n")
for (x in 0:5) {
  cat(sprintf("exp(%d): Actual = %.5f, Approximations: %s\n", x, exp(x), paste(fd(exp, x, 0.01, 3), collapse = ", ")))
}

cat("\nSin Derivative at pi/4:\n")
for (n in 1:5) {
  approx <- fd(sin, pi/4, 0.01, n)
  cat(sprintf("n = %d, Approx = %s, Actual = %.5f\n", n, paste(approx, collapse = ", "), cos(pi/4)))
}

cat("\nIntegration Results:\n")
cat(sprintf("exp(1) - 1 = %.5f\n", exp(1) - 1))
cat(sprintf("Midpoint Rule: %.5f\n", MidpointRule(exp, 0, 1, 7)))
cat(sprintf("Richardson Extrapolation: %.5f\n", MidPointRuleRichardson(exp, 0, 1, 3)))
