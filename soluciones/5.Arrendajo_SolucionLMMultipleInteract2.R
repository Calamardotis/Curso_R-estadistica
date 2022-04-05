#######Ejercicio 2 Modelos lineales con interacción####
library(Stat2Data)
#1.
data("BlueJays")
BJ<-BlueJays
str(BJ)

#2.
#H0: No existe relación entre la masa y 
#la longitud del pico de hembras y machos de arrendajos

#Ha: Existe relación entre la masa
#y la longitud del pico de los arrendajos, y esta difiere entre sexos

#3.

caracol<-lm(BJ$Mass~BJ$BillLength*BJ$KnownSex)
plot(caracol) #Todo bien! --> Prueba a comprobar estas asunciones de otras maneras
#Normalidad:
hist(resid(caracol))
qqnorm(resid(caracol))
#Outliers:
boxplot(BJ$Mass)
boxplot(BJ$BillLength)
#Varianza:

#4.
lm(BJ$Mass~BJ$BillLength*BJ$KnownSex)%>%summary()
#Masa de hembras: 27.26+1.76*Longitud de pico
#Masa de machos: (27.26+26.23)+(1.76-0.98)*longitd pico

#18.47% de la varianza está explicada por el modelo

#5.
plot(BJ$Mass~BJ$BillLength)
plot(BJ$Mass[BJ$KnownSex=="M"]~BJ$BillLength[BJ$KnownSex=="M"],
     col="blue",ylim=c(55,85),xlim=c(22,30), ylab="masa(g)",xlab="longitud pico (mm)",pch=19)
points(BJ$Mass[BJ$KnownSex=="F"]~BJ$BillLength[BJ$KnownSex=="F"],col="red",pch=19)
abline(a=27.26,b=1.76,lwd=5,col="red")
abline(a=27.26+26.2295,b=1.76-0.98,lwd=5,col="blue")
legend("topleft",legend = c("Machos","Hembras"), col=c("blue","red"),
       lwd=5)

