# Función para generar todas las combinaciones posibles de un conjunto de elementos
combina <- function(variables, n) {
  if (n == 1) {
    return(as.list(variables))
  } else {
    combinaciones <- list()
    for (i in 1:(length(variables) - n + 1)) {
      resto <- combina(variables[(i + 1):length(variables)], n - 1)
      for (j in 1:length(resto)) {
        combinaciones[[length(combinaciones) + 1]] <- c(variables[i], resto[[j]])
      }
    }
    return(combinaciones)
  }
}

# Definir la función para calcular el error cuadrático medio (ECM)
calcularEMC <- function(datos, variablesM) {
  # Número de filas para el conjunto de entrenamiento (80%)
  filasE <- round(0.8 * nrow(datos))

  # Selección aleatoria de índices para el conjunto de entrenamiento
  entrenaI <- sample(1:nrow(datos), filasE)

  # Conjuntos de entrenamiento y prueba
  entrenar <- datos[entrenaI, ] 
  prueba <- datos[-entrenaI, ]

  # Ajustar modelos de regresión lineal múltiple para cada combinación de variables
  for (n_variables_combinadas in 1:length(variables_predictoras)) {
    combinaciones_variables <- combina(variablesM, n_variables_combinadas)
    for (combinacion in combinaciones_variables) {
      formula <- as.formula(paste("Peso ~", paste(combinacion, collapse = "+")))
      modelo <- lm(formula, data = entrenar)
      nombre_modelo <- paste(combinacion, collapse = " y ")
      predicciones <- predict(modelo, newdata = prueba)
      error_cuadratico_medio <- mean((prueba$Peso - predicciones)^2)
      print(paste("Error Cuadrático Medio (ECM) para", nombre_modelo, ":", error_cuadratico_medio))
    }
  }
}

corridas <- 10

for (i in 1:corridas) {
	# Leer los datos y calcular el error cuadrático medio
	datos <- read.table("Labrisomus.txt", header = TRUE)
	# Definir las variables predictoras
	variables_predictoras <- c("LongTotal", "LongPatron", "AltMax", "AltMin")
	calcularEMC(datos, variables_predictoras)
}
