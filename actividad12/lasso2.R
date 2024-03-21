# Carga de datos
datos <- read.table("Labrisomus.txt", header = TRUE)

# Carga del paquete necesario para realizar las selecciones
library(MASS)

# Paso a paso
print("Paso a paso")
modelo_paso_a_paso <- lm(y ~ ., data = datos)
modelo_paso_a_paso_seleccionado <- stepAIC(modelo_paso_a_paso)

# Selección hacia adelante
print("Hacia adelante")
modelo_seleccion_hacia_adelante <- lm(y ~ 1, data = datos)
modelo_seleccion_hacia_adelante <- stepAIC(modelo_seleccion_hacia_adelante, direction = "forward", scope = formula(modelo_paso_a_paso))

# Eliminación hacia atrás
print("Hacia atras")
modelo_eliminacion_hacia_atras <- lm(y ~ ., data = datos)
modelo_eliminacion_hacia_atras_seleccionado <- stepAIC(modelo_eliminacion_hacia_atras, direction = "backward")
