####SOLUCIONES 1. Ejercicio Conceptos b�sicos####
#install.packages("datasets")
rm(list=ls())
library(datasets)

#1.
data(ChickWeight)
PesoPollo<-ChickWeight #Renombrar la base de datos

#2.
View(PesoPollo) #Nueva pantalla con base de datos completa
head(PesoPollo) #Primeras filas de la base de datos
str(PesoPollo) #Estructura de la base de datos (# observaciones, 
#variables y tipo de variables )

#3.
mean(PesoPollo$weight) #Media

#4.
var(PesoPollo$weight) #Varianza
range(PesoPollo$weight) #Rango de valores (m�nimo y m�ximo)
sqrt(var(PesoPollo$weight)) #Desviaci�n estandar = raiz cuadrada de la varianza

#5.
summary(PesoPollo$weight)
hist(PesoPollo$weight)
abline(v=63,col="Red")
abline(v=163.8,col="purple")

#6.
str(PesoPollo)
#�Var�a el peso de los pollos en funci�n de la dieta que siguen?
#Ho: El peso no difiere entre pollos con distintas dietas
#Ha: El peso difiere entre pollos con distintas dietas

#7.
boxplot(PesoPollo$weight~PesoPollo$Diet)
