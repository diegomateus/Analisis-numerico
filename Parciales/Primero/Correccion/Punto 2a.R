#Igualo las dos funciones para hallar la interseccion de las dos funciones
interseccion<-function(x)
{
  return(sin(pi*x)-tan(pi*x))
}

#Uso la función recursiva dada por el punto (Los parametros son los dos limites y ña tolerancia del algoritmo)
raiz <- function(a,b,tolerancia){
  cat("\n\n----------------------------------------------------------\n")
  cat(formatC( c("Xn","Xn-1","Error est."), width = -15, format = "f", flag = " "), "\n")
  cat("----------------------------------------------------------\n")
  k = 0
  repeat{
    val1=a
    aux1= (interseccion(a)*(a-b))
    aux2= (interseccion(a)-interseccion(b))
    diferencia=aux1/aux2
    k = k + 1
    a = a - diferencia
    
    cat(c(a,tan(pi*a),diferencia,'\n'))
    b = val1
    
    #En caso que la diferencia del valor absoluto sea menor de la tolerancia quiere decir que se encontro la raiz
    if(abs(a-b) < tolerancia){
      break;
    }
  }
  cat("\nLa raiz en el intervalo es: (",toString(format(a,nsmall = 4)),")", "\nError: ",diferencia, " con un número de iteraciones ", k, "\n" )
}

raiz(9.905,10.1,10^-9)
raiz(21.905,22.1,10^-9)