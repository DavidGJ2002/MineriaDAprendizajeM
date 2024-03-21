# Función para calcular los estimadores ridge
ridge_estimator <- function(X, y, lambda) {
  I <- diag(ncol(X))
  b_ridge <- solve(t(X) %*% X + lambda * I) %*% t(X) %*% y
  return(b_ridge)
}

# Leer la matriz de datos
Archivo <- readline("Ingrese el nombre de su base de datos (Nombre.txt): ")
datos <- read.table(Archivo, header = TRUE)

# Obtener variables independientes y dependiente
Variables <- names(datos)[-1]
V.Dependiente <- names(datos)[1]

# Estandarizar variables independientes
datos[,-1] <- scale(datos[,-1])

# Crear secuencia de lambdas
N <- 10 # Número de lambdas
lambda_values <- seq(0, 1, length.out = N)

# Ajuste de la matriz Betas
Betas <- matrix(NA, nrow = N, ncol = length(Variables)) # Cambiar las filas y columnas
for (i in 1:N) {
  lambda <- lambda_values[i]
  X <- as.matrix(datos[, Variables])
  y <- datos[, V.Dependiente]
  Betas[i, ] <- ridge_estimator(X, y, lambda) # Asignar los coeficientes a la fila correspondiente
}

# Crear dataframe con coeficientes ridge y lambdas
DF <- data.frame(Lambda = lambda_values, Betas)

# Gráfico
matplot(lambda_values, Betas, type = "l", xlab = "Lambda", ylab = "Coeficientes Ridge", 
        main = "Coeficientes Ridge vs Lambda", col = 1:length(Variables))

# Imprimir matriz de coeficientes ridge
print(DF)
