##3.1 Ejercicios ANOVA (cucos)#####
rm(list=ls())

library(DAAG)
data("cuckoos")

#1.

str(cuckoos) #120 observaciones (datos) para cuatro variables
summary(cuckoos) #En la columna de species podemos ver cuantas
#observaciones tiene cada especie. M�s o menos las mismas, 
# pero meadow.pipit tiene muchas m�s observaciones que el resto.

#2.
ck<-subset(cuckoos, cuckoos$species!="meadow.pipit")
str(ck)

#3.
mean(ck$length) #Longitud media de los huevos

range(ck$breadth) #M�n y max de la variable anchura
summary(ck$breadth) #Resumen de los valores de la variable anchura

#4.
#Ho: La especie parasitada no afecta al tama�o de los huevos de cuco
#Ha: La especie parasitada afecta al tama�o de los huevos de cuco


#5.
#Normalidad:
qqnorm(ck$length) #M�s o menos bien, 
qqline(ck$length) #con m�s observaciones ser�a m�s normal
hist(ck$length) #Meh, demoslo por bueno
shapiro.test(ck$length) #No significativo = distribuci�n normal

#Varianza:
library(car)
leveneTest(ck$length~ck$species) #No significativo= varianza homogenea
boxplot(ck$length~ck$species)


#6.
aov(ck$length~ck$species)%>% summary() 
#La especie influye en el tama�o de los huevos
aov(ck$length~ck$species)%>%TukeyHSD()
#Tukey Post-Hoc test nos permite obtener la
#diferencia en la variable estudiada entre parejas de especies  

#Otra manera:
lm(ck$length~ck$species)%>%summary()
#Hedge.sparrow: 23.11
#Pied.wagtail: 23.11-0.2276
#Robin: 23.11-0.56
#???Tree.pipit: 23.11-0.03
#Wren: 23.11-1.99

#7.
str(ck)
#Basiqu�simo
boxplot(ck$length~ck$species, ylab="longitud huevos (mm)",
        xlab="Especies parasitadas")

#Pijadas
library(ggplot2)
library(wesanderson)
ggplot(ck, aes(x=species,y=length,fill=species))+
  geom_boxplot()+
  theme_minimal()+
  labs(x="Especies parasitadas", y="Longitud huevos (mm)")+
  guides(fill=guide_legend(title="Especies"))+
  scale_fill_manual(values=wes_palette(n=5, name="FantasticFox1" ))

##