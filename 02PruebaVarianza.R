# Desarrollado por la Ing. Marcela Parra, Mg.
# Prueba de Varianza para Números Aleatorios del 0 al 1

# 1. Ingresar el valor de alpha (nivel de significancia)
alpha <- 0.05  # Nivel de significancia
cat("Nivel de significancia (alpha):", alpha, "\n")

# 2. Ingresar los valores de r (conjunto de números aleatorios)
x <- c(0.0449, 0.1733, 0.5746, 0.049, 0.8406, 0.8349, 0.92, 0.2564, 0.6015, 0.6694,
       0.3972, 0.7025, 0.1055, 0.1247, 0.1977, 0.0125, 0.63, 0.2531, 0.8297, 0.6483,
       0.6972, 0.9582, 0.9085, 0.8524, 0.5514, 0.0316, 0.3587, 0.7041, 0.5915, 0.2523,
       0.2545, 0.3044, 0.0207, 0.1067, 0.3557, 0.1746, 0.3362, 0.1589, 0.3727, 0.4145)

# 3. Calcular el tamaño de la muestra (n)
n <- length(x)
cat("Tamaño de la muestra (n):", n, "\n")

# 4. Calcular la varianza muestral
varianza <- var(x)
cat("Varianza muestral:", varianza, "\n")

# 5. Calcular el valor de la chi-cuadrada para los límites superior e inferior
# Límite superior (chi-cuadrada para el percentil (1 - alpha/2))
chisup <- qchisq(1 - alpha / 2, df = n - 1)
cat("Chi-cuadrada superior:", chisup, "\n")

# Límite inferior (chi-cuadrada para el percentil (alpha/2))
chiinf <- qchisq(alpha / 2, df = n - 1)
cat("Chi-cuadrada inferior:", chiinf, "\n")

# 6. Calcular los límites inferior y superior de la varianza esperada
# Para una distribución uniforme en [0, 1], la varianza teórica es 1/12
LIV <- chiinf / (12 * (n - 1))
LSV <- chisup / (12 * (n - 1))

cat("Límite inferior de la varianza esperada (LIV):", LIV, "\n")
cat("Límite superior de la varianza esperada (LSV):", LSV, "\n")

# 7. Conclusión: comparar la varianza muestral con los límites calculados
if (varianza >= LIV && varianza <= LSV) {
  cat("Se acepta el conjunto de números aleatorios.\n")
} else {
  cat("Se rechaza el conjunto de números aleatorios.\n")
}
