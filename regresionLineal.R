  Archivo <- readline("Ingrese el nombre de su base de datos (Nombre.txt):  ")
  datos <- read.table(Archivo, header = TRUE); 
  Variables <- names(datos)[-1]
  V.Dependiente <- paste(names(datos)[1], "~", collapse = " ")
  Combinaciones <- function(lista) {
    R <- lapply(1:length(lista), function(i) combn(lista, i, simplify = FALSE))
         unlist(R, recursive = FALSE) 
  }
  Com.Var <- Combinaciones(Variables)
  l <- length(Variables) 
  n <- seq_along(Com.Var) 
  Matriz_ECM <- matrix(numeric(), length(n), 1)
  Variables_del_Modelo <- numeric(length(n))
  Betas <- list() 
  
  for (i in n) {  
    Formula <- as.formula(paste(V.Dependiente, paste(Com.Var[[i]], collapse = "+")))
    modelo <- lm(Formula, data = datos)
    Predicciones <- predict(modelo)
    ECM <- mean((datos[, names(datos)[1]] - Predicciones)^2)
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