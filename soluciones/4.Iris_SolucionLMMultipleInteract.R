
##4.2 Ejercicios Modelos Lineales múltiples con interaccion####

data("iris")
str(iris)
#1. 
#H0: la longitud de los sépalos no está relacionada con la longitud de los pétalos para ninguna de las especies estudiadas
#Ha: la longitud de los sépalos  está relacionada con la longitud de los pétalos al menos en una de las especies estudiadas

#2. 
lm(data=iris, Sepal.Length~Petal.Length*Species)%>%summary()

#Para setosa: y=4.2132+0.5423*x
#Para versicolor: y=(4.2132-1.8056)+(0.5423+0.286)*x
#Para virginica: y=(4.2132-3.1535)+(0.5423+0.4534)*x

#3.
#Existe una relación débil entre la longitud de los pétalos y sépalos 
#en la especie setosa (interpretación de pendientede grupo de referencia)
#De hecho, la relación morfológica entre sépalos y pétalos no difiere entre 
#las especies estudiadas (significancia de pendiente del resto de especies)

#4.
plot(iris$Sepal.Length[iris$Species=="setosa"]~iris$Petal.Length[iris$Species=="setosa"],col="green",xlim=c(1,8),ylim=c(4,8))
points(iris$Sepal.Length[iris$Species=="versicolor"]~iris$Petal.Length[iris$Species=="versicolor"],col="red")
points(iris$Sepal.Length[iris$Species=="virginica"]~iris$Petal.Length[iris$Species=="virginica"],col="black")
abline(a=4.2132,b=0.5423,col="green")
abline(a=4.2132-1.8056,b=0.5423+0.286,col="red")
abline(a=4.2132-3.1535,b=0.5423+0.4534,col="black")

#ó
library(ggplot2)
ggplot(iris, aes(x=Petal.Length, y=Sepal.Length,col=Species))+
  geom_point(size=3)+
  theme_minimal()+
  geom_abline(intercept=4.2132,slope=0.5423,color="pink",size=1.5)+
  geom_abline(intercept=4.2132-1.8056,slope=0.5423+0.286,color="green",size=1.5)+
  geom_abline(intercept=4.2132-3.1535,slope=0.5423+0.4534,color="blue",size=1.5)+
  labs(x="Longitud pétalos (mm)", y="Anchura sépalos (mm)")

#######
