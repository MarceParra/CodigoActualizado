# Datos proporcionados
datos <- c(0.97, 0.11, 0.65, 0.26, 0.98, 0.03, 0.13, 0.89, 0.21, 0.69)

# Realizar la prueba de Kolmogorov-Smirnov
ks_result <- ks.test(datos, "punif", min = 0, max = 1)

# Calcular D+ y D-
n <- length(datos)  # Número de datos
datos_ordenados <- sort(datos)  # Ordenar los datos de menor a mayor

# Función de distribución acumulada teórica para una distribución uniforme
cdf_teorica <- punif(datos_ordenados, min = 0, max = 1)

# Función de distribución acumulada empírica
empirical_cdf <- seq(1, n) / n  # Calcular la CDF empírica

# Calcular D+ (la mayor diferencia positiva entre la CDF empírica y teórica)
D_plus <- max(empirical_cdf - cdf_teorica)

# Calcular D- (la mayor diferencia negativa entre la CDF empírica y teórica)
D_minus <- max(cdf_teorica - (seq(0, n - 1) / n))

# Nivel de confianza
conf_level <- 0.90  # Se puede ajustar el nivel de confianza

# Calcular el valor crítico de D (dependiendo del nivel de confianza y tamaño de muestra)
alpha <- 1 - conf_level
ks_critical_value <- sqrt(-log(alpha / 2) / (2 * n))

# Imprimir los resultados
cat("Resultados de la Prueba de Kolmogorov-Smirnov:\n")
cat("D+ (máxima diferencia positiva):", D_plus, "\n")
cat("D- (máxima diferencia negativa):", D_minus, "\n")
cat("Valor crítico de D para un nivel de confianza del", conf_level * 100, "%:", ks_critical_value, "\n")

# Imprimir la conclusión de la prueba
if (ks_result$statistic < ks_critical_value) {
  cat("Conclusión: Aceptamos la hipótesis nula. Los datos se ajustan a una distribución uniforme.\n")
} else {
  cat("Conclusión: Rechazamos la hipótesis nula. Los datos no se ajustan a una distribución uniforme.\n")
}
