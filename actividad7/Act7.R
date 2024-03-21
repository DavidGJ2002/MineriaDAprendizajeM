# Graficas de los ECM de p
p <- seq(0,1,0.01)
par(mfrow=c(1,2))
n <- 4
plot(p,p*(1-p)/n,type="l",xlab="p",ylab="ECM(p)",col="red")
abline(h = n/(4*(n+sqrt(n))^2),col="blue")
n <- 400
plot(p,p*(1-p)/n,type="l",xlab="p",ylab="ECM(p)",col="red")
abline(h = n/(4*(n+sqrt(n))^2),col="blue")