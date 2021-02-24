
##4.2 Ejercicios Modelos Lineales múltiples sin interaccion####

data("iris")
str(iris)

lm(data=iris, Sepal.Width~Petal.Length+Species)%>%summary()
#Para setosa: y=2.992+0.298*x
#Para versicolor: y=(2.992-1.493)+0.298*x
#Para virginica: y=(2.992-1.674)+0.298*x

plot(iris$Sepal.Width[iris$Species=="setosa"]~iris$Petal.Length[iris$Species=="setosa"],col="green",xlim=c(1,8),ylim=c(1.8,4.8))
points(iris$Sepal.Width[iris$Species=="versicolor"]~iris$Petal.Length[iris$Species=="versicolor"],col="red")
points(iris$Sepal.Width[iris$Species=="virginica"]~iris$Petal.Length[iris$Species=="virginica"],col="black")
abline(a=2.992,b=0.298,col="green")
abline(a=2.992-1.493,b=0.298,col="red")
abline(a=2.992-1.674,b=0.298,col="black")

#ó

ggplot(iris, aes(x=Petal.Length, y=Sepal.Width,col=Species))+
  geom_point(size=3)+
  theme_minimal()+
  geom_abline(intercept=2.992,slope=0.298,color="pink",size=1.5)+
  geom_abline(intercept=2.992-1.493,slope=0.298,color="green",size=1.5)+
  geom_abline(intercept=2.992-1.674,slope=0.298,color="blue",size=1.5)+
  labs(x="Longitud pétalos (mm)", y="Anchura sépalos (mm)")

#######