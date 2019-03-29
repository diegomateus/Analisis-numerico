install.packages("Matrix")#instalar paquete
library(Matrix)
install.packages("PolynomF")#instalar paquete
library(PolynomF)

x=c(100,200,300,400,500,600) 
y=c(-160,-35,-4.2,9,16.9,21.3)

plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Interpolado")

datxn = x[1:4]; datyn = y[1:4]
polinom = poly.calc(datxn,datyn)
polinom


datxn1 = x[3:5]; datyn1 = y[3:5]
polinom1 = poly.calc(datxn1,datyn1)
polinom1(450)

points(datxn,datyn, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(polinom,add=T,from =100,to = 400)

points(datxn1,datyn1, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(polinom1,add=T,from =400,to = 600)

x=c(-pi/10,-pi/5,-(3*pi)/10,-(2*pi)/5,0,pi/10,pi/5,(3*pi)/10,(2*pi)/5) 
y=c(-0.324,-0.7265,-1.376,-3.077,0,0.324,0.7265,1.376,3.077)

plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="tan(x)")

datxn = x[1:9]; datyn = y[1:9]
polinom = poly.calc(datxn,datyn)
polinom

points(datxn,datyn, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito")
curve(polinom,add=T,from =-pi/2,to = pi/2)