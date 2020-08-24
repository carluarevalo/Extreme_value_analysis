#A lo largo de la materia estudiamos distintos metodos para tratar una muestra de datos, enfocandonos en los valores centrales de la 
#distribucion de la misma, ignorando los valores extremos que esta pudiera tener y en muchos casos desechandolos. 
#No obstante, en muchas disciplinas como la economia, la ingenieria o las ciencias ambientales, resulta importantisimo analizar estos valores
#extremos y los eventos que ellos producen por su alto impacto. Encontrar un analisis que lejos de ignorarlos se centre en describirlos, entonces,
#resulta vital. 

#En el presente informe nos propusimos presentar la Teoria de Valores Extremos y trabajar un poco la problematica de su analisis. 
#En particular, como en meteorologia unas de las grandes divas para el estudio de eventos extremos son las olas de calor, decidimos visualizar
#este tema utilizando datos contenidos en un paquete de R llamado heatwaveR (ver Fuentes). Este dataset contiene las temperaturas minimas y
#maximas diarias en grados Celsius registradas en Argel durante el periodo 01/01/1961 - 31/12/2005.
#La ciudad de Argel es la capital de Argelia y se encuentra situada en el litoral mediterraneo del continente africano, en 36°46'35???N 
#3°03'31???E. Como esta ciudad se ubica en el hemisferio norte, particularmente en una zona subtropical y a orillas del mar Mediterraneo, su 
#climatologia es mediterranea maritima, con lo cual las temperaturas cumplen con un regimen normal de temperaturas mas frias en los meses
#invernales de DEF y mas calidas en el verano de JJA, aunque moderadas por la influencia de este gran cuerpo de agua. Es por eso que en verano,
#gracias a las brisas que vienen desde el mar, las maximas no tiende a superar los 30°C; sin embargo, en esta ciudad se dan eventos de olas
#de calor, especialmente cuando soplan los vientos del Sur provenientes del desierto del Sahara, que son muy calidos. 

library(heatwaveR)
datos <- Algiers

library(ggplot2)
ggplot(data=datos, aes(x=t, y=tMax)) + xlab("Años") + ylab("Temperatura") + geom_line(color = "firebrick") + geom_point(color="black") + labs(title = "Temperaturas máximas en Argel durante el período 1961-2005") + theme_minimal()

#Del grafico que muestra la serie de temperaturas maximas para esta ciudad a lo largo de este periodo se observa que las mismas se encuentran 
#entre los valores de 15°C y 35°C, con algunos casos anomalos por debajo y por arriba de estos valores. ¿Que tan anomalos son estos valores,
#especialmente los mas #grandes? ¿Como los encuentro y analizo?

#La Teoria de Valores Extremos es una rama de la estadistica que se enfoca en el estudio de los eventos asociados a las colas de una
#distribucion, tanto la correspondiente a los valores mas altos como la de los mas bajos de la variable aleatoria en estudio. 
#En la practica existen dos aproximaciones a la Teoría de Valores Extremos: el primer metodo se basa en el ajuste de la distribucion de los
#valores maximos o minimos, mientras que en la segunda aproximacion el análisis de los valores extremos se realiza a partir del analisis de
#los valores que exceden cierto umbral. Veamos cada uno por separado.

#MÉTODO 1#

#Calculo los máximos anuales
maximos_anuales <- rep(NA,times=45)
for(i in 1:45){
  anio <- 1960+i
  maximos_anuales[i] <- max(datos$tMax[which(substring(datos$t,1,4)==as.character(anio))])
}
anios <- c(1961:2005)

#Ploteo los máximos anuales
ggplot(data=algiers_maximos, aes(x=anios, y=maximos_anuales)) + xlab("Años") + ylab("Temperatura máxima anual") + geom_point(color="black") + labs(title = "Temperaturas máximas anuales en Argel durante el período 1961-2005") + theme_minimal()

#MÉTODO 2

ggplot(data=datos, aes(x=t, y=tMax)) + xlab("Años") + ylab("Temperatura") + geom_line(color = "firebrick") + geom_point(color="black") + labs(title = "Temperaturas máximas en Argel durante el período 1961-2005") + theme_minimal() + geom_hline(yintercept=35, linetype="dashed", color = "red", size=1.5)

threshold <- datos$tMax[which(datos$tMax>35)]
fechas <- datos$t[which(datos$tMax>35)]  
  
pareto <- data.frame(fechas, threshold)

ggplot(data=pareto, aes(x=fechas, y=threshold)) + xlab("Tiempo") + ylab("Temperatura máxima diaria")+ geom_point(color="black") + labs(title = "Temperaturas máximas en Argel durante el período 1961-2005") + theme_minimal() + geom_hline(yintercept=35, linetype="dashed", color = "red", size=1.5)
