# Instalar y cargar el paquete glmnet si no está instalado
if (!require("glmnet")) {
  install.packages("glmnet")
}
library(glmnet)

# Leer la matriz de datos
Archivo <- readline("Ingrese el nombre de su base de datos (Nombre.txt): ")
datos <- read.table(Archivo, header = TRUE)

# Obtener variables independientes y dependiente
Variables <- names(datos)[-1]
V.Dependiente <- names(datos)[1]

# Estandarizar variables independientes
datos[,-1] <- scale(datos[,-1])

# Crear matriz de diseño y vector de respuesta
X <- as.matrix(datos[,-1])
y <- datos[,1]

# Crear secuencia de lambdas
N <- 10 # Número de lambdas
lambda_values <- seq(0.001, 1, length.out = N)

# Ajuste de la matriz Betas
Betas <- matrix(NA, nrow = N, ncol = length(Variables)) 
for (i in 1:N) {
  lambda <- lambda_values[i]
  lasso_fit <- glmnet(X, y, alpha = 1, lambda = lambda)
  Betas[i, ] <- coef(lasso_fit)[-1]  # Excluir el intercepto
}

# Crear dataframe con coeficientes lasso y lambdas
DF <- data.frame(Lambda = lambda_values, Betas)

# Gráfico
matplot(lambda_values, Betas, type = "l", xlab = "Lambda", ylab = "Coeficientes Lasso", 
        main = "Coeficientes Lasso vs Lambda", col = 1:length(Variables))

# Imprimir matriz de coeficientes lasso
print(DF)
