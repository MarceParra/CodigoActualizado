# Definición del conjunto de números aleatorios (r)
x <- c(0.0449, 0.1733, 0.5746, 0.049, 0.8406, 0.8349, 0.92, 0.2564, 0.6015, 0.6694,
       0.3972, 0.7025, 0.1055, 0.1247, 0.1977, 0.0125, 0.63, 0.2531, 0.8297, 0.6483,
       0.6972, 0.9582, 0.9085, 0.8524, 0.5514, 0.0316, 0.3587, 0.7041, 0.5915, 0.2523,
       0.2545, 0.3044, 0.0207, 0.1067, 0.3557, 0.1746, 0.3362, 0.1589, 0.3727, 0.4145)

# Calcular el tamaño de la muestra (n)
n <- length(x)

# Calcular la media de la muestra
promedio <- mean(x)

# Nivel de significancia alpha (5%)
alpha <- 0.05

# Calcular el valor crítico de zeta para un intervalo de confianza del 95%
zeta <- qnorm(1 - (alpha / 2))

# Calcular los límites inferior y superior para la media esperada (media teórica de la distribución uniforme es 0.5)
li <- 0.5 - (zeta * (1 / sqrt(12 * n)))  # Límite Inferior
ls <- 0.5 + (zeta * (1 / sqrt(12 * n)))  # Límite Superior

# Mostrar los resultados
cat("Media muestral:", promedio, "\n")
cat("Límite Inferior:", li, "\n")
cat("Límite Superior:", ls, "\n")

# Conclusión de la prueba de hipótesis
if ((promedio >= li) && (promedio <= ls)) {
  print("Se acepta el conjunto de números aleatorios")
} else {
  print("Se rechaza el conjunto de números aleatorios")
}
