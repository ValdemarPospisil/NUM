# 1. Určete R pro které platí:
# 0Zp(0Z2*pi e^-ax * sin(x)dx)dalpha = 1.31848

# Funkce pro vnitřní integrál
inner_integral <- function(alpha) {
  sapply(alpha, function(a) {
    integrate(function(x) exp(-a * x) * sin(x), lower = 0, upper = 2 * pi)$value
  })
}

# Funkce pro vnější integrál
outer_integral <- function(p) {
  integrate(function(alpha) inner_integral(alpha), lower = 0, upper = p)$value
}

# Hledání hodnoty p
result <- uniroot(function(p) outer_integral(p) - 1.31848, lower = 0, upper = 10)

cat("Hodnota p, pro kterou platí rovnost:", result$root, "\n")


# Ověření výpočtem
computed_value <- outer_integral(result$root)
cat("Vypočtená hodnota integrálu pro p:", computed_value, "\n")
cat("Rozdíl od požadované hodnoty:", abs(computed_value - 1.31848), "\n")