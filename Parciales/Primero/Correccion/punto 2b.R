newtonDN = function(f, x0, tol, maxiter){
  # Derivada numérica con diferencia central
  fp = function(x) { h = 1e-9
  (f(x+h) - f(x-h)) / (2*h)
  }
  k = 0
  #Par imprimir estado
  cat("\n---------------------------------------------------------------------------\n")
  cat(formatC( c("x_k"," f(x_k)","Error est."), width = -20, format = "f", flag = " "), "\n")
  cat("---------------------------------------------------------------------------\n")
  repeat{
    correccion = f(x0)/fp(x0)
    x1 = x0 - correccion
    dx = abs(correccion)
    # Imprimir iteraciones
    cat(formatC( c(x1 ,f(x1), dx), digits=15, width = -15, format = "f", flag = " "), "\n")
    x0 = x1
    k = k+1
    # until
    if(dx <= tol || k > maxiter ) break;
  }
  cat("---------------------------------------------------------------------------\n")
  if(k > maxiter){
    cat("Se alcanzó el máximo número de iteraciones.\n")
    cat("k = ", k, "Estado: x = ", x1, "Error estimado <= ", correccion)
  } else {
    cat("k = ", k, " x = ", x1, " f(x) = ", f(x1), " Error estimado <= ", correccion) }
}
## --- Pruebas
f = function(x) sin(pi*x)-tan(pi*x)
options(digits = 8)
cat("Con un x0 = 0.77 el resultado es:")
newtonDN(f, 9.905, 1e-10, 100)
cat("\n\nCon un x0 = 2.77 el resultado es:")
newtonDN(f, 21.905, 1e-10, 100)