# Lectura de datos
calcularEMC <- function(datos) {
	# Número de filas para el conjunto de entrenamiento (80%)
	filasE <- round(0.8 * nrow(datos))

	# Selección aleatoria de índices para el conjunto de entrenamiento
	entrenaI <- sample(1:nrow(datos), filasE)

	# Conjuntos de entrenamiento y prueba
	entrenar <- datos[entrenaI, ] 
	prueba <- datos[-entrenaI, ]
	# Definir las variables predictoras
	variables_predictoras <- c("LongTotal", "LongPatron", "AltMax")

	# Crear una lista para almacenar los modelos
	lista_modelos <- list()

	# Ajustar un modelo lineal para cada variable predictora
	for (variable in variables_predictoras) {
		# Construir la fórmula
		vs <- as.formula(paste("Peso ~", variable))
  
  		# Ajustar el modelo
  		modelo <- lm(vs, data = entrenar)
  
  		# Guardar el modelo en la lista
  		lista_modelos[[variable]] <- modelo
  
  		# Imprimir el resumen del modelo
  		cat("Resumen del modelo para", variable, ":\n")
  		#print(summary(modelo))
  		#cat("\n")
		# Predicciones en el conjunto de prueba
		predicciones <- predict(modelo, newdata = prueba)
		error_cuadratico_medio <- mean((prueba$Peso - predicciones)^2)
		print(paste("Error Cuadrático Medio (ECM):", error_cuadratico_medio))
	}


	# Ajuste el modelo de regresión lineal simple
	#modelo <- lm(Peso ~ [LongTotal, LongPatron], data = entrenar)
	#summary(modelo)

	

	
}

corridas <- 1

for (i in 1:corridas) {
	datos <- read.table("Labrisomus.txt", header = TRUE)
	EMC <- calcularEMC(datos)
}

# Gráfica del diagrama de dispersión
#plot(hijos ~ padres, xlab = "Estatura de los padres", ylab = "Estatura de los hijos", data = datos)

# Gráfica de la línea de regresión ajustada
#abline(modelo, col = "red")
