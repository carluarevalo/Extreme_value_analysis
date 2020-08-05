# Ajuste de gev con libreria evir: Extreme Values in R
# https://cran.r-project.org/web/packages/evir/evir.pdf

install.packages("heatwaveR")
install.packages("evir")
library(heatwaveR)
library(evir)

datos <- Algiers

boxplot(datos[,'tMax'], datos[,'tMin'])

# Estimo params con Maxima verosimilitud sobre los datos
gev(datos[,'tMax'], 365)
#$par.ests
#xi      sigma         mu 
#-0.2141458  6.0193876 20.9820117 
ests <- gev(datos[,'tMax'], 365)$par.ests
xi <-ests[1]
sigma <- ests[2]
mu <- ests[3]
# Los datos
xgrid <- seq(-100,100,0.1)
hist(datos[, 'tMax'], prob=T, col='yellowgreen', ylim=c(0, 0.2))
hist(datos[datos$tMax>30, 'tMax'], prob=T, col=rgb(1,0,0,0.5), add=T)
# GEV con parametros encontrados arriba

lines(xgrid, dgev(xgrid,
                 xi = xi,
                 mu = mu,
                 sigma = sigma),
     type='l', lwd=3, col='steelblue')
# Linea vertical en xi
abline(v=xi, lty=3, col='orange')

