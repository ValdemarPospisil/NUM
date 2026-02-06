# Definice funkce f(x)
f <- function(x) -sin(-x^2)

# Nalezení průsečíku r funkce f(x) s osou x na intervalu ⟨1, 2⟩
intersection_function <- function(x) f(x)
r <- uniroot(intersection_function, lower = 1, upper = 2)$root
cat("Průsečík r s osou x je přibližně:", r, "\n")

# Výpočet funkce g(x) = ∫_0^x f(t) dt numericky
library(pracma)
g <- function(x) sapply(x, function(xi) integral(f, 0, xi))

# Vlastní funkce pro numerickou derivaci pomocí centrální diference
numerical_derivative <- function(func, x_vals, h = 1e-5) {
  sapply(x_vals, function(xi) {
    (func(xi + h) - func(xi - h)) / (2 * h)
  })
}

# Vytvoření sekvence x na intervalu ⟨0, r⟩
x_vals <- seq(0, r, length.out = 100)

# Výpočet hodnot f(x), g(x), a h(x)
f_vals <- f(x_vals)
g_vals <- g(x_vals)
h_vals <- numerical_derivative(g, x_vals)

# Vykreslení závislosti f(x) - h(x) na x
plot(x_vals, f_vals - h_vals, type='l', col='blue', lwd=2, xlab='x', ylab='f(x) - h(x)', main='Závislost f(x) - h(x) na x')

