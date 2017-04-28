library(cluster)
library(dplyr)
datos=read.table("C:/Users/JORGE/Desktop/Proyecto/Datos.csv",header = TRUE,sep = ",",dec=".")
row.names(datos)<-datos$Player
X=datos[4:36]
datos=datos[-1]
attach(datos)
CP=prcomp(datos,scale. = TRUE)
biplot(CP)
distancia<-dist(X,method = "euclidean")
hc<-hclust(distancia)
plot(hc)
agrupacion2<-kmeans(X,2)
agrupacion3<-kmeans(X,3)
agrupacion4<-kmeans(X,4)
#Codo de jambu
x<-(nrow(X)-1)*sum(apply(X,2,var))
for(i in 2:20) x[i]<-sum(kmeans(X,centers = i)$withinss)
plot(1:20,x,type = "b",xlab = "Numeros de cluster",ylab = "y",main = "Codo de Jambu")
#Mediante la gráfica se elige el K el cual podría ser  4 ó 5.
agrupacion5<-kmeans(X,5)
#Gráfica de los cluster, selecionando las variables AB y OPS 
plot(X[c("AB","OPS")],col=agrupacion4$cluster)
points(agrupacion4$centers[,c("AB","OPS")],col=1:3, pch=8, cex=2)
#Asignando los cluster correspondiente a cada jugador.
datos$Class<-agrupacion4$cluster
#Visualizando grupos por separado
gr1<-cbind(filter(datos,Class==1))
gr2<-cbind(filter(datos,Class==2))
gr3<-cbind(filter(datos,Class==3))
gr4<-cbind(filter(datos,Class==4))
#Realizando mediciones sobre las siguientes varibales(G,R,HR,AVG,OPS) para clasificar
max(datos$G)
mean(datos$G)
mean(gr1$G)
mean(gr2$G)
mean(gr3$G)
mean(gr4$G)
max(datos$R)
mean(datos$R)
mean(gr1$R)
mean(gr2$R)
mean(gr3$R)
mean(gr4$R)
max(datos$HR)
mean(datos$HR)
mean(gr1$HR)
mean(gr2$HR)
mean(gr3$HR)
mean(gr4$HR)
max(datos$AVG)
mean(datos$AVG)
mean(gr1$AVG)
mean(gr2$AVG)
mean(gr3$AVG)
mean(gr4$AVG)
max(datos$OPS)
mean(datos$OPS)
mean(gr1$OPS)
mean(gr2$OPS)
mean(gr3$OPS)
mean(gr4$OPS)
for (i in 1:547){
  if (datos$Class[i]==1){
    datos$Class[i]="Regular"
  }
  if(datos$Class[i]==2){
    datos$Class[i]="Alto"
  }
  if (datos$Class[i]==3){
    datos$Class[i]="Muy Alto"
  }
  if (datos$Class[i]==4){
    datos$Class[i]="Bajo"
  }
}
#Gráfica de analisis
plot(c(mean(gr1$G),mean(gr2$G),mean(gr3$G),mean(gr4$G)),col="blue", pch=18,cex=3,xlab = "Grupos",ylab = "Juegos",main ="Gráfica1")
abline(h=mean(datos$G),col="red")
plot(c(mean(gr1$R),mean(gr2$R),mean(gr3$R),mean(gr4$R)),col="blue", pch=18,cex=3,xlab = "Grupos",ylab = "Carreras",main ="Gráfica2")
abline(h=mean(datos$R),col="red")
plot(c(mean(gr1$HR),mean(gr2$HR),mean(gr3$HR),mean(gr4$HR)),col="blue", pch=18,cex=3,xlab = "Grupos",ylab = "HR",main ="Gráfica3")
abline(h=mean(datos$HR),col="red")
plot(c(mean(gr1$AVG),mean(gr2$AVG),mean(gr3$AVG),mean(gr4$AVG)),col="blue", pch=18,cex=3,xlab = "Grupos",ylab = "AVG",main ="Gráfica4")
abline(h=mean(datos$AVG),col="red")
plot(c(mean(gr1$OPS),mean(gr2$OPS),mean(gr3$OPS),mean(gr4$OPS)),col="blue", pch=18,cex=3,xlab = "Grupos",ylab = "OPS",main ="Gráfica5")
abline(h=mean(datos$OPS),col="red")
write.csv(datos,file = "TablaF.csv")
#Generando tabla de rendimientos para los equipos
TT=table(datos$Rendimiento,datos$Team)
write.table(TT,file = "Equipos.txt",sep = "\t")





