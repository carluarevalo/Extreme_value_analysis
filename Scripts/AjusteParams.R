# Ajuste de gev con libreria evir: Extreme Values in R
# https://cran.r-project.org/web/packages/evir/evir.pdf

install.packages("heatwaveR")
install.packages("evir")
library(heatwaveR)
library(evir)

datos <- Algiers

boxplot(datos[,'tMax'], datos[,'tMin'])

# Estimo params con Maxima verosimilitud sobre los datos
gev(datos[,'tMax'], 100)
#$par.ests
#xi      sigma         mu 
#-0.2141458  6.0193876 20.9820117 
ests <- gev(datos[,'tMax'], 100)$par.ests
xi <-ests[1]
sigma <- ests[2]
mu <- ests[3]
# Los datos
hist((datos[, 'tMax']-mean(datos[, 'tMax']))/sd(datos[, 'tMax']), prob=T, col='yellowgreen')
# GEV con parametros encontrados arriba
plot(seq(-100,100,0.1), dgev(-seq(-100,100,0.1),
                             xi = xi,
                             mu = mu,
                             sigma = sigma),
     type='l', lwd=3, col='steelblue')
# Linea vertical en xi
abline(v=xi, lty=3, col='orange')

