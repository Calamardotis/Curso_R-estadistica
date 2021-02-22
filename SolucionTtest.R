#####SOLUCIONES 2. Ejercicios T-test####
rm(list=ls())
#0. Necesitamos tomar datos de dos variables: 
#el tamaño de los individuos como variable respuesta y 
#si están o no infectados como variable explicativa
#H0: El tamaño de los renacuajos no difiere en función de su infección
#Ha: El tamaño de los renacuajos difiere en función de su infección


library(Stat2Data)
data("Tadpoles")

#1.
str(Tadpoles) #27 observaciones--> No es suficiente, pero como es un test sencillo,
#tampoco necesitamos un tamaño muestral muy grande

#2.
mean(Tadpoles$Body) #Tamaño medio
range(Tadpoles$Body) #Valor mínimo y maximo de la variable Body
summary(Tadpoles$Body) #Otra manera de obtener un resumen de los valores

#Media del tamaño de los renacuajos en cada uno de los tratamientos:
mean(Tadpoles$Body[Tadpoles$Treatment=="Bd"])
mean(Tadpoles$Body[Tadpoles$Treatment=="Control"])

#Otra manera es crear dos nuevas bases de datos, una para cada tratamiento
#y luego calcular la media del tamaño:
Bd<-subset(Tadpoles, Tadpoles$Treatment=="Bd")
C<-subset(Tadpoles, Tadpoles$Treatment=="Control")
mean(Bd$Body)
mean(Control$Body)


#3.
#Normalidad-
hist(Tadpoles$Body)
qqnorm(Tadpoles$Body)
qqline(Tadpoles$Body) #Mah o menoh, quizá si tuviesemos un mayor 
#tamaño muestral, se vería mejor la distribución normal de los datos

#Homocedasticidad
library(car)
leveneTest(Tadpoles$Body~Tadpoles$Treatment) #Varianzas homogéneas entre grupos


#4.
t.test(Tadpoles$Body~Tadpoles$Treatment)
#Aquellos renacuajos infectados tienen un tamaño 
#significativamente menor que los no infectados
#Los infectados tienen un tamaño medio de 18.94mm 
#mientras que los no infectados tienen un tamaño de 21.02mm

#5.
boxplot(Tadpoles$Body~Tadpoles$Treatment,xlab="Tratamiento",
        ylab="Tamaño corporal (mm)")
