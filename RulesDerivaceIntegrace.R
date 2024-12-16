# Taylorova aproximace exponenciální funkce
ExpTaylor <- function(x, n=1){
  # Inicializace prvního členu a výsledku
  term <- rep(1, length(x))
  res <- term
  
  # Výpočet dalších členů Taylorova rozvoje
  if(n > 1){
    for(i in 2:n){
      term <- term * x / (i-1)   # Další člen polynomu
      res <- res + term          # Přičtení členu do výsledku
    }
  }
  return(res)
}

# Vykreslení aproximace
x <- seq(-3, 3, 0.001)
plot(exp, xlim=c(-3, 3), lwd=4, main="Taylorova aproximace exp(x)", col='black')
for(i in 1:8){
  lines(x, ExpTaylor(x, i), col=colors()[i+1], lwd=4)
}

# Výpočet derivací numericky
FirstDerA <- function(f, x, h) {return((f(x+h)-f(x))/h)}
FirstDerB <- function(f, x, h) {return((f(x+h)-f(x-h))/(2*h))}
SecondDer <- function(f, x, h) {return((f(x+h)-2*f(x)+f(x-h))/(h*h))}

# Vykreslení první derivace
plot(cos, xlim=c(0, 2*pi), lwd=4, main="První derivace sin(x)")
plot(function(x) FirstDerA(sin, x, 1e-8), xlim=c(0, 2*pi), lwd=4, col=2, add=TRUE)

# Porovnání numerických derivací
plot(function(x) FirstDerA(sin, x, 1e-8)-cos(x), xlim=c(0,2*pi), lwd=4, main="Chyba první derivace")
plot(function(x) FirstDerB(sin, x, 1e-8)-cos(x), xlim=c(0,2*pi), lwd=4, col='gray', add=TRUE)
plot(function(x) FirstDerA(sin, x, 1e-7)-cos(x), xlim=c(0,2*pi), lwd=4, col='red', add=TRUE)
plot(function(x) FirstDerB(sin, x, 1e-7)-cos(x), xlim=c(0,2*pi), lwd=4, col='pink', add=TRUE)

# Druhá derivace
plot(function(x) -sin(x), xlim=c(0,2*pi), lwd=4, main="Druhá derivace sin(x)")
plot(function(x) SecondDer(sin, x, 1e-5), xlim=c(0,2*pi), lwd=4, col='green', add=TRUE)

# Pravidlo středu
MidPointRule <- function(f, a, b, n=1){
  h <- (b-a)/n
  return(h * sum(f(h * (1:n) + a - h/2)))
}

# Porovnání přesnosti pravidla středu
cat("Přesné řešení int_a^b sin(x):", cos(0)-cos(pi), "\n")
for(i in 0:20) cat("Pravidlo středu pro n =", 2^i, ":", MidPointRule(sin, 0, pi, 2^i), "\n")

# Simpsonovo pravidlo
SimpsonRule <- function(f, a, b, n=1){
  h <- (b-a)/n
  hpul <- h/2
  suma <- f(a) + f(b)
  xs <- h*(1:n) + a - hpul
  suma <- suma + 4*sum(f(xs))
  if(n > 1){
    xl <- (xs + hpul)[-n]
    suma <- suma + 2*sum(f(xl))
  }
  return(suma*h/6)
}

# Porovnání Simpsonova pravidla
for(i in 0:20) cat("Simpsonovo pravidlo pro n =", 2^i, ":", SimpsonRule(sin, 0, pi, 2^i), "\n")
