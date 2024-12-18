# Se desarrolló por la Ing. Marcela Parra, Mg.
# Prueba de Varianza para Números Aleatorios del 0 al 1

# 1. Ingresar el valor de n (tamaño de la muestra)
n = 100
cat("Tamaño de la muestra (n):", n, "\n")

# 2. Definir el valor de m (número de intervalos)
m = sqrt(n)
cat("Número de intervalos (m):", m, "\n")

# 3. Ingresar el valor de alpha (nivel de significancia)
alpha = 0.05
cat("Nivel de significancia (alpha):", alpha, "\n")

# 4. Ingresar los valores de r (números aleatorios)
r <- c(0.347, 0.832, 0.966, 0.472, 0.797, 0.101, 0.696, 0.966, 0.404, 0.603, 
       0.993, 0.371, 0.729, 0.067, 0.189, 0.977, 0.843, 0.562, 0.549, 0.992, 
       0.674, 0.628, 0.055, 0.494, 0.494, 0.235, 0.178, 0.775, 0.797, 0.252, 
       0.426, 0.054, 0.022, 0.742, 0.674, 0.898, 0.641, 0.674, 0.821, 0.19, 
       0.46, 0.224, 0.99, 0.786, 0.393, 0.461, 0.011, 0.977, 0.246, 0.881, 
       0.189, 0.753, 0.73, 0.797, 0.292, 0.876, 0.707, 0.562, 0.562, 0.821, 
       0.112, 0.191, 0.584, 0.347, 0.426, 0.057, 0.819, 0.303, 0.404, 0.64, 
       0.37, 0.314, 0.731, 0.742, 0.213, 0.472, 0.641, 0.944, 0.28, 0.663, 
       0.909, 0.764, 0.999, 0.303, 0.718, 0.933, 0.056, 0.415, 0.819, 0.444, 
       0.178, 0.516, 0.437, 0.393, 0.268, 0.123, 0.945, 0.527, 0.459, 0.652)
cat("Datos de la muestra:\n", r, "\n")

# 5. Graficar histograma de los datos
hist(r, main = "Histograma de los Números Aleatorios", xlab = "Valor", ylab = "Frecuencia")

# 6. Crear los límites de los intervalos
interval_size = 1 / m
breaks <- seq(0, 1, by = interval_size)

# 7. Crear la tabla de frecuencias observadas
data_cutr <- cut(r, breaks = breaks, include.lowest = TRUE, right = FALSE)
freq_obs_table <- table(data_cutr)
cat("Frecuencias observadas por intervalo:\n", freq_obs_table, "\n")

# 8. Graficar las frecuencias observadas en barra
barplot(freq_obs_table, main = "Frecuencias Observadas", col = "lightblue")

# 9. Calcular la frecuencia esperada (en una distribución uniforme debería ser igual para todos los intervalos)
freq_esperada <- n / m
cat("Frecuencia esperada por intervalo:", freq_esperada, "\n")

# 10. Calcular el chi-cuadrado para cada intervalo
chi_calculado <- ((freq_obs_table - freq_esperada)^2) / freq_esperada
cat("Chi-cuadrado calculado por intervalo:\n", chi_calculado, "\n")

# 11. Sumar los valores de chi-cuadrado para obtener el chi-cuadrado total
chi_total <- sum(chi_calculado)
cat("Chi-cuadrado total:", chi_total, "\n")

# 12. Obtener el valor crítico de chi-cuadrado
gl <- m - 1  # Grados de libertad
chi_critico <- qchisq(1 - alpha, df = gl)
cat("Valor crítico de Chi-cuadrado:", chi_critico, "\n")

# 13. Comparar chi_total con chi_critico
if (chi_total < chi_critico) {
    conclusion <- "No se rechaza la hipótesis nula. Los datos se distribuyen uniformemente."
} else {
    conclusion <- "Se rechaza la hipótesis nula. Los datos no se distribuyen uniformemente."
}

# 14. Imprimir la conclusión
cat("Conclusión: ", conclusion, "\n")
