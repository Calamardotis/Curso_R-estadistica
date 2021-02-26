
##Ejercicio final libre resolución####

##Solución Ejer libre- estrategias vitales#####
rm(list=ls())
library(tidyr)
library(lmtest)
library(Stat2Data)
data("BirdNest")
BN<-BirdNest

str(BN)
BN$Closed.<-as.factor(BN$Closed.)


#Pregunta 1: ¿Qué  estrategia de inversión parental tienen las especies estudiadas?
#Es decir, en el marco de estas estrategias  ¿podemos ver que hay una relación negativa 
#entre el tamaño de los adultos y el número de huevos que ponen?
#Ho: El tamaño de los padres no está relacionado con el número de huevos puestos
#Ha: El tamaño de los padres está relacionado con el número de huevos puestos
#Predicción: A mayor tamaño, menos huevos (estrategia K)
#A menor tamaño, más huevos (esrategia R)

#Asunciones:
m1<-lm(BN$No.eggs~BN$Length) 
plot(m1) #Parece que podríamos tener un outlier... El valor de más de 12 huevos puestos por un individuo es unn poco extraño, 
#y biológicamente cuestionable, vamos a quitarlo.

BN<-subset(BN,BN$No.eggs<12)

m2<-lm(BN$No.eggs~BN$Length)
#Checkamos las asunciones de nuevo sin el outlier:
plot(m2) #Mejor, pero vamos a hacerlo también manualmente:
hist(resid(m2)) #Distribución normal
qqnorm(resid(m2))

bptest(m2) #Varianza homogenea

boxplot(BN$No.eggs) #No parece haber valores atípicos
boxplot(BN$Length) #No parece haber valores atípicos


#Vamos allá con el modelo:

lm(BN$No.eggs~BN$Length)%>%summary()
#Por cada incremento de la longitud en 1 cm, el número de huevos disminuye en 0.1.
#Es decir, existe una relación negativa entre el número de huevos y la longitud de la especie. 

#Esto confirma nuestra hipótesis, aceptamos la Ha y rechazamos la Ho. 

#Sin embargo, el R2 de nuestro modelo nos indica que este modelo solo explica el 
#20% de la varianza observada en el número de huevos.
#Eso quiere decir que hay otros factores que están influyendo en el 
#número de huevos que cada especie o individuo tiene.

plot(BN$No.eggs~BN$Length,pch=19,xlab="Longitud (Cm)",ylab="Número de huevos")
abline(a=6.22,b=-0.098)

#Pregunta 2: ¿El tiempo invertido en la incubación de los huevos está relacionado
#con el tiempo invertido en el cuidado de las crías, y depende esto del posible
#riesgo al que se enfrentan los padres al estar en el nido?
#Es decir, ¿podemos ver una relación entre el tiempo de cuidado de los huevos y
#el tiempo de cuidado de las crías, y difiere esta entre aquellos nidos cerrados y abiertos? 
#Ho: El tiempo de incubación y de cuidado de las crías no está relacionado, 
#independientemente del riesgo que corran los padres en el nido (i.e. del tipo de nido abierto o cerrado)
#Ha: El tiempo de incubación y de cuidado de las crías está relacionado, 
#y varía en función del riesgo que corran los padres en el nido 

#Predicción: Las aves con nidos cerrados pasarán más tiempo en el nido cuidando de sus crías, porque no corren peligro al hacerlo
#Mientras que las aves con nidos abiertos, pasarán menos tiempo en el nido cuidando de sus crías por el riesgo que ello supone.

m3<-lm(BN$Nestling~BN$Incubate*BN$Closed.)

hist(resid(m3))#Distribución normal
qqnorm(resid(m3)) #Distrib. normal
bptest(m3) #Varianza homogénea
boxplot(BN$Nestling)#No outliers
boxplot(BN$Incubate)#No outliers

plot(m3)

#Vamos allá con el modelo:
summary(m3)

#El grupo de referencia es Closed=0 -> Nido abierto
#Los pájaros que tienen nido abierto, por cada día más incubando los huevos,
#cuidaban sus crías 1.4 días más.Es decir, sí existe una relación entre el
#tiempo de incubación y de cría en las especies que tienen nidos abiertos.
#La ecuación de la línea de regresión del grupo de Nidos Abiertos es: 
#TiempodeCría = -5.93 + 1.44 * TiempoIncubación

#El grupo de nidos cerrados, su intercepto es estadísticamente distinto 
#del del grupo de referencia, y su pendiente es casi estadísticamente distinta 
#del de grupo de referencia. Quizá con más datos este valor sería significativo, 
#así que como es solo un valor arbitrario, vamos a decir que sí existe un efecto 
#distinto para las aves con nidos cerrados entre el tiempo de incubación y de cría.
#La ecuación de la línea de regresión del grupo de NIDOS CERRADOS es:
#TiempodeCría = (-5.93+14.7) + (1.44-0.8429)*TiempoIncubación



plot(BN$Nestling[BN$Closed.=="0"]~BN$Incubate[BN$Closed.=="0"],
     ylim=c(5,25),xlim=c(10,18),col="red",pch=19)
points(BN$Nestling[BN$Closed.=="1"]~BN$Incubate[BN$Closed.=="1"],col="green",pch=19)
abline(a=-5.93,b=1.44,col="red")
abline(a=-5.93+14.7,b=-0.84+1.44,col="green")
legend("topleft",legend = c("Nido abierto","Nido cerrado"), col=c("red","green"),
       lwd=5)

#Respecto a nuestra predicción: 
#Las aves con nido cerrado muestran una relación más suave entre el tiempo de incubación y de cría, pero más alta de media,
#Eso quiere decir que cuidan a sus crías mayor tiempo, 
#pero que esta no está fuertemente relacionada con el tiempo que emplean en incubación
#Las aves con nidos abiertos sí que muestran una fierte relación entre el numero de días incubando y cuidando a las crías. 


#El R2 de nuestro modelo es de casi 0.5, indicando que casi el 50 de la varianza de el tiempo de cría está explicado por nuestro modelo. 


#¡Bien hecho!#