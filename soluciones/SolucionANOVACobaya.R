##3.2 Ejercicios ANOVA (Cobayas)#####
rm(list=ls())
library("datasets") #cargar librería 
data("ToothGrowth") #Cargar los datos en tu environment

#1.
dientecitos<-ToothGrowth #Dejemos el inglés y renombrar nuestra base de datos

str(dientecitos)
head(dientecitos)
#Base de datos perfecta, tenemos 60 observaciones y 
#las 3 variables que necesitamos para nuestro Two-way ANOVA

#AYAYAY!!!! Pero espera... Si nuestras variables explicativas son 
#realmente categorías (aka tratamientos), necesitan ser 
#variables factoriales, no numericas. Necesitamos cambiar el tipo de 
#datos para la variable "dose", porque pese a que son numeros,
#son grupos de tratamientos.
dientecitos$dose<-as.factor(dientecitos$dose)


#2.
#Normalidad:
hist(dientecitos$len) #Mas o menos
qqnorm(dientecitos$len) #Con más datos seguramente se viese más claro,
qqline(dientecitos$len) #pero la mayoría de datos caen sobre la diagonal
shapiro.test(dientecitos$len) #No significativo == Datos distribuidos normalmente

#Varianza: Como tenemos dos variables explicativas, 
#tenemos que testar que la varianza es homogenea entre 
#los grupos de ambas variables
library(car)
leveneTest(dientecitos$len~dientecitos$supp) #No significativo== Varianza homogenea
leveneTest(dientecitos$len~dientecitos$dose) #No significativo== Varianza homogenea

#Outliers:
boxplot(dientecitos$len~dientecitos$supp)
boxplot(dientecitos$len~dientecitos$dose)#Ningun outlier que comprobar! Genial

#3.
library(tidyr)
aov(dientecitos$len~dientecitos$supp*dientecitos$dose)%>% summary()
#Los suplementos que damos a las cobayas afectan de distinta forma a la longitud de los dientes
#La dosis que damos a las cobayas afectan de distinta forma a la longitud tb
#La interacción entre suplementos y dosis es significativa...
#Eso implica que las distintas combinaciones de suplementos vitaminicos
#y dosis afectan de distinta manera a la longitud de los dientes

#necesitamos ver que dosis y combinación de tratamientos 
#provoca este efecto en la longitud de los dientes
aov(dientecitos$len~dientecitos$supp*dientecitos$dose)%>% TukeyHSD()

#4.
ggplot(dientecitos, aes(x=supp, y=len, fill=dose))+
  geom_boxplot()+
  theme_minimal()+
  scale_fill_manual(values=c("gray90","grey50","black"))+
  labs(x="Suplemento",y="Longitud dientes (mm)")

#ó

ggplot(dientecitos, aes(x=dose, y=len, fill=supp))+
  geom_boxplot()+
  theme_minimal()+
  scale_fill_manual(values=c("orange1","olivedrab3"))+
  labs(x="Dosis (mg)",y="Longitud dientes (mm)")



#####