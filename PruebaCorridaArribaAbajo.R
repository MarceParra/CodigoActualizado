# Datos proporcionados
datos <- c(0.34, 0.67, 0.46, 0.11, 0.83, 0.62, 0.22, 0.19, 0.96, 0.05, 
           0.99, 0.58, 0.47, 0.49, 0.78, 0.34, 0.79, 0.59, 0.39, 0.42, 
           0.99, 0.42, 0.18, 0.37, 0.37, 0.05, 0.75, 0.31, 0.72, 0.02, 
           0.73, 0.73, 0.06, 0.74, 0.79, 0.74, 0.18, 0.67, 0.29, 0.21)

# Número de datos
n <- length(datos)
cat("Número de datos:", n, "\n")

# Ordenar los datos
datos_ordenados <- sort(datos)

# Calcular el número de corridas observadas (S)
# Una corrida es un cambio de tendencia (de 0 a 1 o de 1 a 0) en los valores por encima o debajo de la media
S <- sum(diff(datos_ordenados > mean(datos_ordenados)) != 0)
cat("Número de corridas observadas (S):", S, "\n")

# Calcular la media y varianza de S usando las fórmulas para la media y varianza de una secuencia de corridas
media_S <- (2 * n - 1) / 3
varianza_S <- (16 * n - 29) / 90

cat("Media de S (esperada):", media_S, "\n")
cat("Varianza de S (esperada):", varianza_S, "\n")

# Calcular el valor observado de Z
valor_observado_Z <- abs((S - media_S) / sqrt(varianza_S))
cat("Valor observado de Z:", valor_observado_Z, "\n")

# Definir el nivel de confianza
alpha <- 0.05  # Nivel de significancia (5%)
z_critico <- qnorm(1 - alpha / 2)  # Valor crítico para un intervalo de confianza del 95%
cat("Valor crítico (z_critico) para alpha =", alpha, ":", z_critico, "\n")

# Imprimir conclusión
if (valor_observado_Z <= z_critico) {
  cat("Conclusión: No se rechaza la hipótesis nula. Los números parecen ser aleatorios.\n")
} else {
  cat("Conclusión: Se rechaza la hipótesis nula. Los números no parecen ser aleatorios.\n")
}
