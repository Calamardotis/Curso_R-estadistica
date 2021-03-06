
##Ejercicio final libre resoluci�n####

##Soluci�n Ejer libre- estrategias vitales#####
rm(list=ls())
library(tidyr)
library(lmtest)
library(Stat2Data)
data("BirdNest")
BN<-BirdNest

str(BN)
BN$Closed.<-as.factor(BN$Closed.)


#Pregunta 1: �Qu�  estrategia de inversi�n parental tienen las especies estudiadas?
#Es decir, en el marco de estas estrategias  �podemos ver que hay una relaci�n negativa 
#entre el tama�o de los adultos y el n�mero de huevos que ponen?
#Ho: El tama�o de los padres no est� relacionado con el n�mero de huevos puestos
#Ha: El tama�o de los padres est� relacionado con el n�mero de huevos puestos
#Predicci�n: A mayor tama�o, menos huevos (estrategia K)
#A menor tama�o, m�s huevos (esrategia R)

#Asunciones:
m1<-lm(BN$No.eggs~BN$Length) 
plot(m1) #Parece que podr�amos tener un outlier... El valor de m�s de 12 huevos puestos por un individuo es unn poco extra�o, 
#y biol�gicamente cuestionable, vamos a quitarlo.

BN<-subset(BN,BN$No.eggs<12)

m2<-lm(BN$No.eggs~BN$Length)
#Checkamos las asunciones de nuevo sin el outlier:
plot(m2) #Mejor, pero vamos a hacerlo tambi�n manualmente:
hist(resid(m2)) #Distribuci�n normal
qqnorm(resid(m2))

bptest(m2) #Varianza homogenea

boxplot(BN$No.eggs) #No parece haber valores at�picos
boxplot(BN$Length) #No parece haber valores at�picos


#Vamos all� con el modelo:

lm(BN$No.eggs~BN$Length)%>%summary()
#Por cada incremento de la longitud en 1 cm, el n�mero de huevos disminuye en 0.1.
#Es decir, existe una relaci�n negativa entre el n�mero de huevos y la longitud de la especie. 

#Esto confirma nuestra hip�tesis, aceptamos la Ha y rechazamos la Ho. 

#Sin embargo, el R2 de nuestro modelo nos indica que este modelo solo explica el 
#20% de la varianza observada en el n�mero de huevos.
#Eso quiere decir que hay otros factores que est�n influyendo en el 
#n�mero de huevos que cada especie o individuo tiene.

plot(BN$No.eggs~BN$Length,pch=19,xlab="Longitud (Cm)",ylab="N�mero de huevos")
abline(a=6.22,b=-0.098)

#Pregunta 2: �El tiempo invertido en la incubaci�n de los huevos est� relacionado
#con el tiempo invertido en el cuidado de las cr�as, y depende esto del posible
#riesgo al que se enfrentan los padres al estar en el nido?
#Es decir, �podemos ver una relaci�n entre el tiempo de cuidado de los huevos y
#el tiempo de cuidado de las cr�as, y difiere esta entre aquellos nidos cerrados y abiertos? 
#Ho: El tiempo de incubaci�n y de cuidado de las cr�as no est� relacionado, 
#independientemente del riesgo que corran los padres en el nido (i.e. del tipo de nido abierto o cerrado)
#Ha: El tiempo de incubaci�n y de cuidado de las cr�as est� relacionado, 
#y var�a en funci�n del riesgo que corran los padres en el nido 

#Predicci�n: Las aves con nidos cerrados pasar�n m�s tiempo en el nido cuidando de sus cr�as, porque no corren peligro al hacerlo
#Mientras que las aves con nidos abiertos, pasar�n menos tiempo en el nido cuidando de sus cr�as por el riesgo que ello supone.

m3<-lm(BN$Nestling~BN$Incubate*BN$Closed.)

hist(resid(m3))#Distribuci�n normal
qqnorm(resid(m3)) #Distrib. normal
bptest(m3) #Varianza homog�nea
boxplot(BN$Nestling)#No outliers
boxplot(BN$Incubate)#No outliers

plot(m3)

#Vamos all� con el modelo:
summary(m3)

#El grupo de referencia es Closed=0 -> Nido abierto
#Los p�jaros que tienen nido abierto, por cada d�a m�s incubando los huevos,
#cuidaban sus cr�as 1.4 d�as m�s.Es decir, s� existe una relaci�n entre el
#tiempo de incubaci�n y de cr�a en las especies que tienen nidos abiertos.
#La ecuaci�n de la l�nea de regresi�n del grupo de Nidos Abiertos es: 
#TiempodeCr�a = -5.93 + 1.44 * TiempoIncubaci�n

#El grupo de nidos cerrados, su intercepto es estad�sticamente distinto 
#del del grupo de referencia, y su pendiente es casi estad�sticamente distinta 
#del de grupo de referencia. Quiz� con m�s datos este valor ser�a significativo, 
#as� que como es solo un valor arbitrario, vamos a decir que s� existe un efecto 
#distinto para las aves con nidos cerrados entre el tiempo de incubaci�n y de cr�a.
#La ecuaci�n de la l�nea de regresi�n del grupo de NIDOS CERRADOS es:
#TiempodeCr�a = (-5.93+14.7) + (1.44-0.8429)*TiempoIncubaci�n



plot(BN$Nestling[BN$Closed.=="0"]~BN$Incubate[BN$Closed.=="0"],
     ylim=c(5,25),xlim=c(10,18),col="red",pch=19)
points(BN$Nestling[BN$Closed.=="1"]~BN$Incubate[BN$Closed.=="1"],col="green",pch=19)
abline(a=-5.93,b=1.44,col="red")
abline(a=-5.93+14.7,b=-0.84+1.44,col="green")
legend("topleft",legend = c("Nido abierto","Nido cerrado"), col=c("red","green"),
       lwd=5)

#Respecto a nuestra predicci�n: 
#Las aves con nido cerrado muestran una relaci�n m�s suave entre el tiempo de incubaci�n y de cr�a, pero m�s alta de media,
#Eso quiere decir que cuidan a sus cr�as mayor tiempo, 
#pero que esta no est� fuertemente relacionada con el tiempo que emplean en incubaci�n
#Las aves con nidos abiertos s� que muestran una fierte relaci�n entre el numero de d�as incubando y cuidando a las cr�as. 


#El R2 de nuestro modelo es de casi 0.5, indicando que casi el 50 de la varianza de el tiempo de cr�a est� explicado por nuestro modelo. 


#�Bien hecho!#