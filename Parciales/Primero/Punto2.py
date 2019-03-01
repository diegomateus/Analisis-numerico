import math

def f(x):
    return math.tan (180* x)

def metodo_secante(x_0,x_1,err):
    it = 0
    f_x = f(x_0)
    x_2 = 0
    while abs(f_x) > err :
        x_2 = x_0 - f(x_0)*((x_1-x_0)/(f(x_1)-f(x_0)))
        x_0 = x_1
        x_1 = x_2
        f_x = f(x_2)
        it = it + 1
    return (x_2, it)

x_0 = [6,3]
x_1 = [3,0.6]
errores = [1e-9]
for x in range(0,2):
    for error in errores:
