print("Al ingresar su base de datos recuerde que las columnas deben tener nombre")
Archivo <- readline("Ingrese el nombre de su base de datos (Nombre.txt):  ")
datos <- read.table(Archivo, header = TRUE) 
filas <- nrow(datos)
Variables <- names(datos)[-1]
V.Dependiente <-  paste(names(datos)[1], "~", collapse = " ")
Combinaciones <- function(lista) {
	R <- lapply(1:length(lista), function(i) combn(lista, i, simplify = FALSE))
	unlist(R, recursive = FALSE) 
}
Com.Var <- Combinaciones(Variables)
l <- length(Variables) 
n <- seq_along(Com.Var)
Betas <- list() 
Matriz_ECM <- matrix(numeric(), length(n), 1)
Variables_del_Modelo <- numeric(length(n))
Predicciones <- numeric(filas)

for (i in n) {
	for (k in 1:filas) {			
		D.Ent <- datos[-k,]
		N.Var <- paste(Com.Var[[i]], collapse = " y ")
		Formula <- as.formula(paste(V.Dependiente, paste(Com.Var[[i]], collapse = "+")))
		modelo <- lm(Formula, data = D.Ent)
		Predicciones[k] <- predict(modelo, datos[k,])
		Matriz_ECM[i,1] <- mean((datos[, names(datos)[1]] - Predicciones)^2)
	}
Coef <- matrix(numeric(), length(Com.Var[[i]]) +1, 1)  
Coef[,1] <- coef(modelo) 
Betas[[i]] <- Coef
Variables_del_Modelo[i] <- N.Var
}
DF <- data.frame(Variables_del_Modelo, Matriz_ECM)
View(DF[order(DF$Matriz_ECM), ])
Betas

#print(paste("El modelo con el ECM más chico es", Variables_del_Modelo[which.min(Matriz_ECM)], "con", min(Matriz_ECM)))