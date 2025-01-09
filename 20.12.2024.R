
library(deSolve)

# Funkce pro obecné řešení diferenciální rovnice
solve_ode <- function(t, y, parms) {
  return(list(y - t))  # Odpovídá y' = y - t
}

# Počáteční podmínky a rozsah
y0 <- 1  # Odhadneme počáteční hodnotu y(0)
times <- seq(0, 1, by = 0.01)  # Časový rozsah

# Numerické řešení pomocí ode
initial_conditions <- c(y = y0)
sol <- ode(y = initial_conditions, times = times, func = solve_ode, parms = NULL)

# Extrahování výsledku y(t)
y_vals <- sol[, "y"]

# Numerická integrace výsledné funkce y(t) v intervalu [0, 1]
integral_value <- trapz(times, y_vals)

# Hledání správného počátečního stavu, aby platil požadovaný integrál
target_integral <- 2
epsilon <- 1e-6  # Tolerance

# Funkce pro vyhledávání správné počáteční podmínky
find_initial_y <- function(y0) {
  initial_conditions <- c(y = y0)
  sol <- ode(y = initial_conditions, times = times, func = solve_ode, parms = NULL)
  y_vals <- sol[, "y"]
  integral_value <- trapz(times, y_vals)
  return(integral_value - target_integral)
}

# Použití uniroot k nalezení správné počáteční podmínky
result <- uniroot(find_initial_y, lower = 0, upper = 10)

cat("Správná počáteční podmínka y(0):", result$root, "\n")
