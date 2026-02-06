# spočte koeficienty aproximačního polynomu stupně n-1 pro zadané body x, y
LSA <- function(x, y, n){
  A <- matrix(0, n, n)
  b <- numeric(n)
  for(i in 1:n){
    for(j in 1:n){
      A[i,j] <- sum(x^{i+j-2})
    }
    b[i] <- sum(y*x^{i-1})
  }
  return(solve(A, b))
}

# spočte koeficienty aproximačního polynomu stupně n-1 pro zadané body x, y
LSAef <- function(x, y, n){
  X <- matrix(1, nrow=n, ncol=length(x))
  for(i in 2:n)  X[i, ] <- X[i-1, ]*x
  return(c(solve(X%*%t(X), x%*%y)))
}

# spočte hodnoty polynomu s koeficienty a v bodě  x
Horner <- function(x, a){
  n <- length(a)
  res <- a[n]
  for(i in (n-1):1){
    res <- res * x + a[i]
  }
  return(res)
}



n <- 100
x <- runif(n, -1, 1)
y <- exp(x)

plot(x,y)
m <- 4
coef <- LSA(x, y, m)
coefLASef <- LSAef(x, y, m)
print(coef)
plot(function(x) Horner(x, coef), add = TRUE, col='red', lw = 2, xlim = c(-1,1))
