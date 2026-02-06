p <- c(0.1, 0.2, 0.28, 0.41, 0.98, 1.39, 1.93, 2.75, 3.01, 3.51)
a <- c(0.089, 0.127, 0.144, 0.163, 0.189, 0.198, 0.206, 0.208, 0.209, 0.210)

# Vykreslení původních dat
plot(p, a, col='red', xlab="p [MPa]", ylab="a", main="Závislost a na p")
points(p, a, col='red', pch=19)

model <- nls(a ~ am * b * p / (1 + b * p), start = list(am = 0.21, b = 0.5))

params <- coef(model)
am <- params["am"]
b <- params["b"]
cat("Parametry modelu: am =", am, ", b =", b, "\n")

# Přidání regresní křivky do grafu
curve(am * b * x / (1 + b * x), from = min(p), to = max(p), col='blue', add=TRUE, lwd=2)

# Interpolace polynomem nižšího stupně (např. 3. stupeň)
interp_model <- lm(a ~ poly(p, 3, raw=TRUE))  # Použití polynomu 3. stupně
poly_fit <- function(x) predict(interp_model, newdata = data.frame(p = x))
curve(poly_fit(x), from = min(p), to = max(p), col='green', add=TRUE, lwd=2, lty=2)

# Vykreslení rozdílu mezi oběma křivkami
plot(p, am * b * p / (1 + b * p) - poly_fit(p), type='l', col='purple', xlab="p [MPa]", ylab="Rozdíl", main="Rozdíl mezi křivkami")

# Úprava funkce intersection_function pro nový interval hledání
intersection_function <- function(x) am * b * x / (1 + b * x) - poly_fit(x)

# Vyhledání průsečíku na menším intervalu, kde se křivky protínají
intersection_point <- uniroot(intersection_function, lower = 0.5, upper = 3)

cat("Největší průsečík je přibližně při p =", intersection_point$root, "\n")

# Vykreslení průsečíku do původního grafu
points(intersection_point$root, am * b * intersection_point$root / (1 + b * intersection_point$root), col="purple", pch=19)

# Legenda
legend("bottomright", legend = c("Data", "Nelineární regresní model", "Interpolační polynom", "Průsečík"),
       col = c("red", "blue", "green", "purple"), lty = c(NA, 1, 2, NA), pch = c(19, NA, NA, 19), lwd = 2)
