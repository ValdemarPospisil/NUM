# Mějme čtvercovou matici řádu 50.
#   - V prvním řádku jsou její hodnoty 100.
#   - V ostatních okrajových buňkách jsou její hodnoty 0.
#   - Pro vnitřní buňky platí, že jsou aritmetickým průměrem sousedních buněk:
#     A[i,j] <- 0.25*(A[i-1,j] + A[i+1,j] + A[i,j-1] + A[i,j+1])
#  Určete hodnoty vnitřních buněk.
  


# Definice velikosti matice
n <- 50

# Inicializace matice s okrajovými podmínkami
A <- matrix(0, n, n)
A[1, ] <- 100  # První řádek je 100

max_iter <- 10000

for (iter in 1:max_iter) {
  
  for (i in 2:(n - 1)) {
    for (j in 2:(n - 1)) {
      A[i, j] <- 0.25 * (A[i - 1, j] + A[i + 1, j] + A[i, j - 1] + A[i, j + 1])
    }
  }
}

# Zobrazení výsledné matice
print(A)

# Vykreslení teplotní mapy
library(fields)
image.plot(1:n, 1:n, A, main="Hodnoty vnitřních buněk matice", xlab="Sloupec", ylab="Řádek", axes=TRUE)
axis(1, at=seq(0, n, by=5), labels=seq(0, n, by=5))
axis(2, at=seq(0, n, by=5), labels=seq(0, n, by=5))