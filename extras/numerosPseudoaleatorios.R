"""
Generación de números Pseudialeatorios a partir de congruencia Lineal

Este método se utiliza para generar secuencias de números pseudoaleatorios en un rango especifico
Se define a partir de la siguiente formula:
	X_(i+1)=aX_(i)+ c mod m
donde X serán los números pseudoaleatorios generados
	m debe ser mayor que cero 
	a debe de estar entre cero y m que será una constante multiplicadora
	c debe de estar entre cero y m que será una constante de incremento
	X_0 es la semilla, de tal forma que: 0<=X_0<m

"""

generarNumeros <- function(x0, m, a, c, noA){
	numA <- numeric(noA)
	numA[1] <- x0
	for (i in 2:noA){
		numA[i] <- ((numA[i-1] * a)+ c) %% m
	}
	return (numA)
}

numerosAleatorios<-generarNumeros(0.3,0.2,0.1,.345,10)
print(numerosAleatorios)
