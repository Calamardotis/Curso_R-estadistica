
##4.1 Ejercicios Modelos Lineales simples####
rm(list=ls())
library(datasets)
data("iris")

#1.
str(iris) #Tamaño muestral: 150 observacions
head(iris)

#2.
summary(iris$Petal.Length)
mean(iris$Petal.Length)

summary(iris$Sepal.Width)
range(iris$Sepal.Width)

#3.
#H0: No existe una relación morfológica entre los sépalos y los pétalos de flores de iris
#Ha: Existe una relación morfológica entre los sépalos y los pétalos de flores de iris


#4.
#Normalidad:
morf<-lm(iris$Sepal.Width~iris$Petal.Length)
hist(resid(morf)) #La hostia! Que perfección de histograma...
qqnorm(resid(morf))
qqline(resid(morf))
shapiro.test(resid(morf)) #No significativo= distribucion normal de residuos

#Outliers
boxplot(iris$Sepal.Width) #Hay outliers, pero entran dentro de lo biologicamente
#aceptable (hay sépalos que pueden tener 4.5 cm de anchura)
boxplot(iris$Petal.Length) #Chachi

#Varianza:
plot(lm(iris$Sepal.Width~iris$Petal.Length))



#5.
lm(iris$Sepal.Width~iris$Petal.Length) %>%summary()
#Ecuación: anchura de los sepalos =intecepto + pendiente* longitud de petalos
#anchura de los sepalos= 3.455-0.106* longitud de los petalos
#R2= 0.178 <- 17.8% de la varianza de nuestros datos
#está explicada por nuestro modelo

#6.
plot(iris$Sepal.Width~iris$Petal.Length)
abline(a=3.455,b=-0.106)

ggplot(iris, aes(x=Petal.Length, y=Sepal.Width))+
  geom_point(size=3)+
  theme_minimal()+
  geom_abline(intercept=3.45487,slope=-0.10579,color="green",size=1.5)+
  labs(x="Longitud pétalos (mm)", y="Anchura sépalos (mm)")

######
