print("Al ingresar su base de datos recuerde que las columnas deben tener nombre")
Archivo <- readline("Ingrese el nombre de su base de datos (Nombre.txt):  ")
datos <- read.table(Archivo, header = TRUE); #set.seed(123)
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
Sep <- round(0.8 * nrow(datos))

for (i in n) {
	M.Smp <- sample(1:nrow(datos), Sep)
	D.Ent <- datos[M.Smp, ] 
	D.Pru <- datos[-M.Smp, ]
   	Formula <- as.formula(paste(V.Dependiente, paste(Com.Var[[i]], collapse = "+")))
	modelo <- lm(Formula, data = D.Ent)
	Predicciones <- predict(modelo, newdata = D.Pru)
	ECM <- mean((D.Pru[, names(datos)[1]] - Predicciones)^2)
	Matriz_ECM[i,] <- ECM
	Coef <- matrix(numeric(), length(Com.Var[[i]]) +1, 1)  
	Coef[,1] <- coef(modelo) 
	Betas[[i]] <- Coef
	N.Var <- paste(Com.Var[[i]], collapse = " y ")
	Variables_del_Modelo[i] <- N.Var    	
}
DF <- data.frame(Variables_del_Modelo, Matriz_ECM)
View(DF[order(DF$Matriz_ECM), ])
Betas