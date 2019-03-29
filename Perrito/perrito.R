##Analisis numerico 

install.packages("Matrix")#instalar paquete
library(Matrix)
install.packages("PolynomF")#instalar paquete
library(PolynomF)
#indice de Jaccard

cont = 0
Jaccard = function(f,xI,yI){
  errorRelativo = (yI - f(xI))/yI
  tol = 0.05
  if (errorRelativo < tol)
    cont = cont + 1;
}


##Puntos contorno superior
x=c(1,2,6,8.1,10,13,17.6,20,24.5,25,27.5,28,30) 
y=c(3,3.7,4.5,6.69,7.12,6.7,4.45,7,5.6,5.87,4.1,4.3,3)

#puntos contorno inferior
x1 = c(1,5,6,8,9,10.5,11,12,15.7,16.6,17.6,21,22,24,26,30)
y1 = c(3,3,2.3,2,1.82,2.33,2.22,2,1.9,1.6,2,2,1.67,2,1.66,3)
#puntos no seleccionados
xn=c(5,7.5,23.5,26.5,29)   
yn=c(3.9,5.7,6.1,5.15,4.1)                           

#Realizamos el ajuste 

datsx = x1[1:2]; datsy = y1[1:2]
AjusteBajo = poly.calc(datsx,datsy)
AjusteBajo

datsx1 = x1[2:4]; datsy1 = y1[2:4]
AjusteBajo1 = poly.calc(datsx1,datsy1)
AjusteBajo1


datsx2 = x1[3:6]; datsy2 = y1[3:6]
AjusteBajo2 = poly.calc(datsx2,datsy2)
AjusteBajo2

datsx3 = x1[5:7]; datsy3 = y1[5:7]
AjusteBajo3 = poly.calc(datsx3,datsy3)
AjusteBajo3

datsx4 = x1[6:9]; datsy4 = y1[6:9]
AjusteBajo4 = poly.calc(datsx4,datsy4)
AjusteBajo4

datsx5 = x1[9:11]; datsy5 = y1[9:11]
AjusteBajo5 = poly.calc(datsx5,datsy5)
AjusteBajo5


datsx6 = x1[11:13]; datsy6 = y1[11:13]
AjusteBajo6 = poly.calc(datsx6,datsy6)
AjusteBajo6

datsx7 = x1[12:14]; datsy7 = y1[12:14]
AjusteBajo7 = poly.calc(datsx7,datsy7)
AjusteBajo7

datsx8 = x1[13:16]; datsy8 = y1[13:16]
AjusteBajo8 = poly.calc(datsx8,datsy8)
AjusteBajo8

DatosX = x[1:3]; DatosY = y[1:3]
Ajuste_Polinomio = poly.calc(DatosX,DatosY)
Ajuste_Polinomio
DatosX1 = x[2:4]; DatosY1 = y[2:4]
Ajuste_Polinomio1 = poly.calc(DatosX1,DatosY1)
Ajuste_Polinomio1(7.5)


DatosX2 = x[3:5]; DatosY2 = y[3:5]
Ajuste_Polinomio2 = poly.calc(DatosX2,DatosY2)
Ajuste_Polinomio2

DatosX3 = x[4:6]; DatosY3 = y[4:6]
Ajuste_Polinomio3 = poly.calc(DatosX3,DatosY3)
Ajuste_Polinomio3

DatosX4 = x[5:8]; DatosY4 = y[5:8]
Ajuste_Polinomio4 = poly.calc(DatosX4,DatosY4)
Ajuste_Polinomio4

DatosX5 = x[7:9]; DatosY5 = y[7:9]
Ajuste_Polinomio5 = poly.calc(DatosX5,DatosY5)
Ajuste_Polinomio5

DatosX6 = x[8:10]; DatosY6 = y[8:10]
Ajuste_Polinomio6 = poly.calc(DatosX6,DatosY6)
Ajuste_Polinomio6

DatosX7 = x[9:11]; DatosY7 = y[9:11]
Ajuste_Polinomio7 = poly.calc(DatosX7,DatosY7)
Ajuste_Polinomio7

DatosX8 = x[10:13]; DatosY8 = y[10:13]
Ajuste_Polinomio8 = poly.calc(DatosX8,DatosY8)
Ajuste_Polinomio8


plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
points(xn[1:5],yn[1:5] , pch=19, cex=1, col = "green", asp=1,xlab="X", ylab="Y", main="Perrito")
points(DatosX,DatosY, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(Ajuste_Polinomio,add=T,from =1,to = 2)

points(x[4:7],y[4:7], pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(Ajuste_Polinomio1,add=T,from =2,to = 8.1)
Jaccard(Ajuste_Polinomio1,5,3.9)
Jaccard(Ajuste_Polinomio1,7.5,5.7)
cont = cont +1
cat("hola ",cont)

points(DatosX2,DatosY2, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(Ajuste_Polinomio2,add=T,from =8.1,to = 10)

points(DatosX3,DatosY3, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(Ajuste_Polinomio3,add=T,from =10,to = 13)

points(DatosX4,DatosY4, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(Ajuste_Polinomio4,add=T,from =13,to = 17.6)

points(DatosX5,DatosY5, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(Ajuste_Polinomio5,add=T,from =17.6,to = 24.5)

points(DatosX6,DatosY6, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(Ajuste_Polinomio6,add=T,from =24.5,to = 25)
Jaccard(Ajuste_Polinomio6,23.5,6.1)


points(DatosX7,DatosY7, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(Ajuste_Polinomio7,add=T,from =25,to = 27.5)
Jaccard(Ajuste_Polinomio7,26.5,5.15)

points(DatosX8,DatosY8, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(Ajuste_Polinomio8,add=T,from =27.5,to = 30)
Jaccard(Ajuste_Polinomio8,29,4.1)

points(datsx,datsy, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(AjusteBajo,add=T,from =1,to = 5)

points(datsx1,datsy1, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(AjusteBajo1,add=T,from =5,to = 8)

points(datsx2,datsy2, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(AjusteBajo2,add=T,from =8,to = 10.5)

points(datsx3,datsy3, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(AjusteBajo3,add=T,from =10.5,to = 11)

points(datsx4,datsy4, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(AjusteBajo4,add=T,from =11,to = 15.7)

points(datsx5,datsy5, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(AjusteBajo5,add=T,from =15.7,to = 17.6)

points(datsx6,datsy6, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(AjusteBajo6,add=T,from =17.6,to = 22)

points(datsx7,datsy7, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(AjusteBajo7,add=T,from =22,to = 24)

points(datsx8,datsy8, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(AjusteBajo8,add=T,from =24,to = 30)