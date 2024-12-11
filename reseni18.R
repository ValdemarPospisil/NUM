
# Nechť A je čvtercová matice řádu 'n' a 'y'  je vektor délky n, kde n = 10.
# Prvky matice A a vektoru y jsou definovány následovně:
  # A[i,j] = cos((i -1) * j) - j
  # y[i] = sin(i)
  # kde i a j jsou celá čísla od 1 do n.

# Definice matice A a vektoru y
n <- 10
A <- matrix(0, n, n)
y <- numeric(n)

# Naplnění matice A a vektoru y
for (i in 1:n) {
  for (j in 1:n) {
    A[i, j] <- cos((i - 1) * j) - j
  }
  y[i] <- sin(i)
}

# 1. Řešte soustavy lineárních rovnic Ax = y, kde A je matice definovaná výše,
    # x je neznámý vektor a y je vektor definovaný výše
x <- solve(A, y)
cat("Řešení soustavy x:\n")
print(x)

# 2. Interpolujte polynomem  p(x) množinu bodů danou vektory x a y
interpolace <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + 
                    I(x^6) + I(x^7) + I(x^8) + I(x^9))

# Definice polynomu
polynom <- function(t) {
  predict(interpolace, newdata = data.frame(x = t))
}

# a) Vykreslete p(x) a určtěte, kde se graf p(x) protíná s osou y.
curve(polynom, from = min(x), to = max(x), col = "blue", lwd = 2,
      main = "Interpolovaný polynom p(x)", xlab = "x", ylab = "p(x)")
points(x, y, col = "red", pch = 19)


# Určení průsečíku s osou y
intersect_y <- polynom(0)
cat("Průsečík s osou y: p(0) =", intersect_y, "\n")

# 2b. Výpočet určitého integrálu
integral_value <- integrate(polynom, min(x), max(x))$value
cat("Určitý integrál p(x) od", min(x), "do", max(x), "je", integral_value, "\n")
