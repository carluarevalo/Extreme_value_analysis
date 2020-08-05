# Ajuste de gev con libreria evir: Extreme Values in R
# https://cran.r-project.org/web/packages/evir/evir.pdf

#install.packages("heatwaveR")
#install.packages("evir")
library(heatwaveR)
library(evir)

datos <- Algiers

boxplot(datos[,'tMax'], datos[,'tMin'])

# Estimo params con Maxima verosimilitud sobre los datos
# Ver consola para detalles:
ajuste <- gev(datos[,'tMax'], 365)
ajuste
#$par.ests
#xi      sigma         mu 
#-0.2141458  6.0193876 20.9820117
max_datos <- ajuste$data
# Lo guardo en variables
ests <- ajuste$par.ests
xi <-ests[1]
sigma <- ests[2]
mu <- ests[3]
# Los datos
xgrid <- seq(0,100,0.1)
# Histograma de TODOS los datos
hist(max_datos, prob=T, col='yellowgreen', xlim=c(0, 50), ylim=c(0, 0.2))
# Histograma de datos > 30 (por poner una referencia)
#hist(datos[datos$tMax>35, 'tMax'], prob=T, col=rgb(1,0,0,0.5), add=T)
# GEV con parametros encontrados arriba
lines(xgrid, dgev(xgrid,
                 xi = xi,
                 mu = mu,
                 sigma = sigma),
     type='l', lwd=3, col='steelblue')
# $par.ses en vez de $par.ests (no se que es)
# lines(xgrid, dgev(xgrid,
#                   xi = ajuste$par.ses[1],
#                   mu = ajuste$par.ses[2],
#                   sigma = ajuste$par.ses[3]),
#       type='l', lwd=3, col='orange')
# Linea vertical en xi
abline(v=mu, lwd=3, lty=3, col='orange')
legend('topleft', c('Datos','Ajuste GEV', 'mu_hat'),
       lty=c(1,1,3), lwd=3,
       col=c('yellowgreen', 'steelblue', 'orange'))

