#####SOLUCIONES 2. Ejercicios T-test####
rm(list=ls())
#0. Necesitamos tomar datos de dos variables: 
#el tama�o de los individuos como variable respuesta y 
#si est�n o no infectados como variable explicativa
#H0: El tama�o de los renacuajos no difiere en funci�n de su infecci�n
#Ha: El tama�o de los renacuajos difiere en funci�n de su infecci�n


library(Stat2Data)
data("Tadpoles")

#1.
str(Tadpoles) #27 observaciones--> No es suficiente, pero como es un test sencillo,
#tampoco necesitamos un tama�o muestral muy grande

#2.
mean(Tadpoles$Body) #Tama�o medio
range(Tadpoles$Body) #Valor m�nimo y maximo de la variable Body
summary(Tadpoles$Body) #Otra manera de obtener un resumen de los valores

#Media del tama�o de los renacuajos en cada uno de los tratamientos:
mean(Tadpoles$Body[Tadpoles$Treatment=="Bd"])
mean(Tadpoles$Body[Tadpoles$Treatment=="Control"])

#Otra manera es crear dos nuevas bases de datos, una para cada tratamiento
#y luego calcular la media del tama�o:
Bd<-subset(Tadpoles, Tadpoles$Treatment=="Bd")
C<-subset(Tadpoles, Tadpoles$Treatment=="Control")
mean(Bd$Body)
mean(Control$Body)


#3.
#Normalidad-
hist(Tadpoles$Body)
qqnorm(Tadpoles$Body)
qqline(Tadpoles$Body) #Mah o menoh, quiz� si tuviesemos un mayor 
#tama�o muestral, se ver�a mejor la distribuci�n normal de los datos

#Homocedasticidad
library(car)
leveneTest(Tadpoles$Body~Tadpoles$Treatment) #Varianzas homog�neas entre grupos


#4.
t.test(Tadpoles$Body~Tadpoles$Treatment)
#Aquellos renacuajos infectados tienen un tama�o 
#significativamente menor que los no infectados
#Los infectados tienen un tama�o medio de 18.94mm 
#mientras que los no infectados tienen un tama�o de 21.02mm

#5.
boxplot(Tadpoles$Body~Tadpoles$Treatment,xlab="Tratamiento",
        ylab="Tama�o corporal (mm)")
