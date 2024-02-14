calcularEMC <-function(datos){

	# Número de filas para el conjunto de entrenamiento (80%)
	filasE <- round(0.8 * nrow(datos))

	# Selección aleatoria de índices para el conjunto de entrenamiento
	entrenaI <- sample(1:nrow(datos), filasE)

	# Conjuntos de entrenamiento y prueba
	entrenar <- datos[entrenaI, ] 
	prueba <- datos[-entrenaI, ]

	# Ajuste del modelo de regresión lineal simple
	modelo <- lm(hijos ~ padres, data = entrenar)
	summary(modelo)

	# Predicciones en el conjunto de prueba
	predicciones <- predict(modelo, newdata = prueba)
	#print(predicciones)

	error_cuadratico_medio <- mean((prueba$hijos - predicciones)^2)
	print(paste("Error Cuadrático Medio (ECM):", error_cuadratico_medio))
}

corridas <- 1

for (i in 1:corridas) {
	# Leer los datos y calcular el error cuadrático medio
	datos <- read.table("pearson.txt", header = TRUE)
		calcularEMC(datos)
}


# Gráfica del diagrama de dispersión
#plot(hijos ~ padres, xlab = "Estatura de los padres", ylab = "Estatura de los hijos", data = datos)

# Gráfica de la línea de regresión ajustada
#abline(modelo, col = "red")
