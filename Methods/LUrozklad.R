# Matice koeficientů
A <- matrix(c(2, 4, 3,
              3, 1, 2,
              -1, 2, 3), nrow = 3, byrow = TRUE)

# Vektor pravé strany
b <- c(1, 2, 3)

# Řešení soustavy
x <- solve(A, b)
print(x)


# Načtení balíčku pracma
library(pracma)

# Matice koeficientů
A <- matrix(c(2, 4, 3,
              3, 1, 2,
              -1, 2, 3), nrow = 3, byrow = TRUE)

# Vektor pravé strany
b <- c(1, 2, 3)

# LU rozklad matice A
lu_decomp <- lu(A)

# Vypsání L a U matice
L <- lu_decomp$L
U <- lu_decomp$U

print("L matice:")
print(L)
print("U matice:")
print(U)

# Řešení soustavy pomocí LU rozkladu
y <- forwardsolve(L, b)
x <- backsolve(U, y)
print("Řešení soustavy x:")
print(x)
