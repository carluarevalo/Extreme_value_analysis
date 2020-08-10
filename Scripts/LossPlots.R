# Ajuste de gev con libreria evir: Extreme Values in R
# https://cran.r-project.org/web/packages/evir/evir.pdf

#install.packages("heatwaveR")
#install.packages("evir")
library(heatwaveR)
library(evir)
library(plotly)

datos <- Algiers

ajuste <- gev(datos[,'tMax'], 365)
#$par.ests
#xi      sigma         mu 
#-0.2141458  6.0193876 20.9820117
max_datos <- ajuste$data
# Lo guardo en variables
ests  <- ajuste$par.ests
xi    <- ests[1]
sigma <- ests[2]
mu    <- ests[3]

log_loss <- function(Z, mu, sigma, xi){
    if(xi==0){
        l <- -m * log(sigma) - sum( (Z - mu)/sigma ) - sum( exp( - ((Z - mu)/sigma) ) )
        return(l)
    }
    oper <- 1 + xi * (Z - mu)/sigma
    assumption <- all(oper > 0)
    if( ! assumption ){
        #print("Error! Invalid assumption")
        return(NA)
    }
    m <- length(Z)
    l <- -m * log(sigma) - ( 1 + 1/xi ) * sum( log( oper ) ) - sum( oper ^(-1/xi) )
    return(l)
}
log_loss(max_datos, mu, sigma, xi)

# grillas
mus <- seq(mu-1, mu+1, length.out = 100)
sigmas <- seq(max(0, sigma-1), sigma+1, length.out = 100)
xis <- seq(xi-0.1, xi+0.1, length.out = 100)


# xi fija
Nmus <- length(mus)
Nsigmas <- length(sigmas)
losses <- matrix(rep(0, Nmus*Nsigmas) , nrow=Nmus, ncol=Nsigmas)
rownames(losses) <- mus
colnames(losses) <- sigmas

for(i in 1:Nmus){
    for(j in 1:Nsigmas){
        losses[i, j] <- log_loss(max_datos, mus[i], sigmas[j], xi)
    }
}

#persp(losses)    
fig <- plot_ly(x=~mus, y=~sigmas, z = ~losses)
fig <- fig %>% add_surface(contours = list(
    z = list(
        show=TRUE,
        usecolormap=TRUE,
        highlightcolor="#ff0000",
        project=list(z=TRUE)
    )
))
fig <- fig %>% layout(
    title = "Loss function, fixed xi",
    scene = list(
        xaxis = list(title = "Mu"),
        yaxis = list(title = "Sigma"),
        zaxis = list(title = "Loss")
    ))
fig


## sigma fija
mus <- seq(mu-1, mu+1, length.out = 100)
sigmas <- seq(max(0, sigma-1), sigma+1, length.out = 100)
xis <- seq(xi-0.1, xi+0.1, length.out = 100)

Nmus <- length(mus)
Nxis <- length(xis)
losses <- matrix(rep(0, Nmus*Nxis) , nrow=Nmus, ncol=Nxis)
rownames(losses) <- mus
colnames(losses) <- xis

for(i in 1:Nmus){
    for(k in 1:Nxis){
        losses[i, k] <- log_loss(max_datos, mus[i], sigma, xis[k])
    }
}

#persp(losses)    
fig <- plot_ly(x=mus, y=xis, z = ~losses)
fig <- fig %>% add_surface(contours = list(
    z = list(
        show=TRUE,
        usecolormap=TRUE,
        highlightcolor="#ff0000",
        project=list(z=TRUE)
    )
))
fig <- fig %>% layout(
    title = "Loss function, fixed sigma",
    scene = list(
        xaxis = list(title = "Mu"),
        yaxis = list(title = "Xi"),
        zaxis = list(title = "Loss")
    ))
fig

## Mu fijo
mus <- seq(mu-1, mu+1, length.out = 100)
sigmas <- seq(max(0, sigma-1), sigma+1, length.out = 100)
xis <- seq(xi-0.1, xi+0.1, length.out = 100)

Nsigmas <- length(sigmas)
Nxis <- length(xis)
losses <- matrix(rep(0, Nsigmas*Nxis) , nrow=Nsigmas, ncol=Nxis)
rownames(losses) <- sigmas
colnames(losses) <- xis

for(j in 1:Nsigmas){
    for(k in 1:Nxis){
        losses[j, k] <- log_loss(max_datos, mu, sigmas[j], xis[k])
    }
}

#persp(losses)    
fig <- plot_ly(x=~sigmas, y=~xis, z = ~losses)
fig <- fig %>% add_surface(contours = list(
    z = list(
        show=TRUE,
        usecolormap=TRUE,
        highlightcolor="#ff0000",
        project=list(z=TRUE)
    )
))
fig <- fig %>% layout(
    title = "Loss function, fixed mu",
    scene = list(
        xaxis = list(title = "Sigma"),
        yaxis = list(title = "Xi"),
        zaxis = list(title = "Loss")
    ))
fig

