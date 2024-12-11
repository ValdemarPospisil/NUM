# Hornerovo schéma pro výpočet hodnoty polynomu
Horner <- function(coef, x) {
  n <- length(coef)
  res <- coef[n]
  for (i in (n-1):1) {
    res <- res * x + coef[i]
  }
  return(res)
}

# Analytický výpočet integrálu polynomu
IntegratePolynom <- function(coef, a, b) {
  n <- length(coef)
  coef_int <- coef / (1:n)  # Integrace polynomu
  F_a <- Horner(coef_int, a)  # Hodnota v bodě a
  F_b <- Horner(coef_int, b)  # Hodnota v bodě b
  return(F_b - F_a)  # Výsledek integrace
}

# Monte Carlo geometrická metoda
GeomMethod <- function(f, a, b, h, n) {
  x <- runif(n, a, b)  # Náhodné x v [a, b]
  y <- runif(n, 0, h)  # Náhodné y v [0, h]
  return(h * (b - a) * sum(y < f(x)) / n)  # Monte Carlo odhad
}

# Monte Carlo metoda průměrů
AveMethod <- function(f, a, b, n) {
  return((b - a) * mean(f(runif(n, a, b))))  # Výpočet průměrů
}

# Gaussovská kvadratura s 2 uzly
Gauss2 <- function(f, a, b) {
  x <- 1 / sqrt(3)
  slope <- (b - a) / 2
  intercept <- (a + b) / 2
  return(slope * sum(f(slope * c(x, -x) + intercept)))
}

# Gaussovská kvadratura se 3 uzly
Gauss3 <- function(f, a, b) {
  x <- sqrt(0.6)
  x <- c(-x, 0, x)
  w <- c(5, 8, 5) / 9
  slope <- (b - a) / 2
  intercept <- (a + b) / 2
  return(slope * sum(w * f(slope * x + intercept)))
}

# Gaussovská kvadratura se 4 uzly
Gauss4 <- function(f, a, b) {
  x1 <- sqrt(3/7 - sqrt(6/5) * 2/7)
  x2 <- sqrt(3/7 + sqrt(6/5) * 2/7)
  x <- c(x1, -x1, x2, -x2)
  w1 <- (18 + sqrt(30)) / 36
  w2 <- (18 - sqrt(30)) / 36
  w <- c(w1, w1, w2, w2)
  
  slope <- (b - a) / 2
  intercept <- (a + b) / 2
  return(slope * sum(w * f(slope * x + intercept)))
}

# Testovací data
coef <- runif(8)  # Polynomiální koeficienty
a <- 2
b <- 7

# Výpočet analytického integrálu polynomu
cat("Integrál polynomu analyticky:", IntegratePolynom(coef, a, b), "\n")

# Gaussovská kvadratura pro polynom
cat("Gauss2:", Gauss2(function(x) Horner(coef, x), a, b), "\n")
cat("Gauss3:", Gauss3(function(x) Horner(coef, x), a, b), "\n")
cat("Gauss4:", Gauss4(function(x) Horner(coef, x), a, b), "\n")

# Test geometrické a průměrné metody pro sin(x) na [0, pi]
na <- 1000  # Počet opakování

# Geometrická metoda s n = 10000
x <- replicate(na, GeomMethod(sin, 0, pi, 1, 10000))
cat("GeomMethod (n=10000):", mean(x), "±", sd(x), "\n")

# Geometrická metoda s n = 40000
x <- replicate(na, GeomMethod(sin, 0, pi, 1, 40000))
cat("GeomMethod (n=40000):", mean(x), "±", sd(x), "\n")

# Metoda průměrů s n = 40000
x <- replicate(na, AveMethod(sin, 0, pi, 40000))
cat("AveMethod (n=40000):", mean(x), "±", sd(x), "\n")

# Další test pro symetrickou funkci
x <- replicate(na, AveMethod(function(x) (sin(x) + sin(pi - x)) / 2, 0, pi, 40000))
cat("Symetrická funkce:", mean(x), "±", sd(x), "\n")
