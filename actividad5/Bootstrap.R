Iteraciones <- as.integer(readline("Ingrese la cantidad de iteraciones que desea:  "))
print("Al ingresar su base de datos recuerde que las columnas deben tener nombre")
Archivo <- readline("Ingrese el nombre de su base de datos (Nombre.txt):  ")

set.seed(4347)
datos <- read.table(Archivo, header = TRUE) 
Variables <- names(datos)[-1]
V.Dependiente <-  paste(names(datos)[1], "~", collapse = " ")
Combinaciones <- function(lista) {
	R <- lapply(1:length(lista), function(i) combn(lista, i, simplify = FALSE))
	unlist(R, recursive = FALSE) 
}
Com.Var <- Combinaciones(Variables)
l <- length(Variables)
n <- seq_along(Com.Var)
m <- length(n)
Matriz_ECM <- matrix(numeric(), m, Iteraciones)  
Variables_del_Modelo <- numeric(m)
Betas <- list() 
PromBetas <- list()

for (i in n) {
Coef <- matrix(numeric(), length(Com.Var[[i]]) +1, Iteraciones)  
	for (k in 1:Iteraciones) {
		NumAlea <- sample(1:nrow(datos), replace=TRUE) 
		N.NumAlea <- setdiff(1:nrow(datos), NumAlea)
		D.Ent <- datos[NumAlea,] 
		D.Pru <- datos[N.NumAlea,]
		N.Var <- paste(Com.Var[[i]], collapse = " y ")
		Formula <- as.formula(paste(V.Dependiente, paste(Com.Var[[i]], collapse = "+")))
		modelo <- lm(Formula, data = D.Ent)
		Predicciones <- predict(modelo, newdata = D.Pru)
		ECM <- mean((D.Pru[, names(datos)[1]] - Predicciones)^2)
	  Matriz_ECM[i,k] <- ECM
		Coef[,k] <- coef(modelo)
	}
Variables_del_Modelo[i] <- N.Var
Betas[[i]] <- Coef
PromBetas[[i]] <- rowMeans(Betas[[i]])
}
Promedio_ECM <- rowMeans(Matriz_ECM)
DF <- data.frame(Variables_del_Modelo, Promedio_ECM)
View(DF[order(DF$Promedio_ECM), ])
PromBetas


# print(paste("El modelo con el ECM más chico es", Variables_del_Modelo[which.min(Promedio_ECM)], "con", min(Promedio_ECM)))