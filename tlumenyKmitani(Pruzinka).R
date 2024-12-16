# Definice diferenciální rovnice
f <- function(t,X){
  # X obsahuje pozici X[1] a rychlost X[2]
  return(c(X[2], -X[1]-0.1*X[2]))
}

# Eulerova metoda (přesnost 1. řádu)
Euler <- function(x, y , f, h){
  # Přímý krok metody Euler
  return(y+h*f(x,y))
}

# Runge-Kutta 2. řádu (přesnost 2. řádu)
RK2 <- function(x, y , f, h){
  hhalf <- 0.5*h
  # Výpočet prostředního bodu a přibližného koncového bodu
  return(y+h*f(x+hhalf,y+h*f(x+hhalf,y+hhalf*f(x,y))))
}

# Runge-Kutta 4. řádu (přesnost 4. řádu)
RK4 <- function(x, y , f, h){
  hhalf <- 0.5*h
  k1 <- f(x,y)
  k2 <- f(x+hhalf, y+hhalf*k1)
  k3 <- f(x+hhalf, y+hhalf*k2)
  k4 <- f(x+h, y+h*k3)
  # Výpočet váženého průměru kroků
  return(y+h*(k1+2*(k2+k3)+k4)/6)
}

# Nastavení počátečních podmínek

# Maximální čas, počet kroků a krok h
tMAX <- 100
n <- 1000
h <- tMAX/n
# Časový vektor
t <- seq(0, tMAX, h)
n <- n+1
# Inicializace matice výsledků
X <- matrix(0, nrow = n, ncol = 2)
X[1,1] <- 100  # Počáteční hodnota pozice x(0)
X[1,2] <- 0    # Počáteční rychlost v(0)

# Výpočet Eulerovou metodou
for(i in 2:n) X[i,] <- Euler(t, X[i-1,], f, h)
plot(t, X[,1], col='red', type='l', main="Numerické řešení diferenciální rovnice")

# Výpočet metodou Runge-Kutta 2. řádu
Xrk2 <- X
for(i in 2:n) Xrk2[i,] <- RK2(t, Xrk2[i-1,], f, h)
lines(t, Xrk2[,1], col='blue', type='l')

# Výpočet metodou Runge-Kutta 4. řádu
Xrk4 <- X
for(i in 2:n) Xrk4[i,] <- RK4(t, Xrk4[i-1,], f, h)
lines(t, Xrk4[,1], col='violet', type='l')

# Analytické řešení
lines(t, 100*exp(-0.1*t/2)*cos(t), col='green', type='l')
