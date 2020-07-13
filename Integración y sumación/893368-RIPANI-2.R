'
Trabajo práctico #2 (Ejercicio 1 - Integración)
Análisis Numérico
Máximo Ripani
Facultad de Ciencias Económicas de Buenos Aires
'


# 1. Integración

options(digits=20)

setwd("/Users/mac/Facultad/Analisis Numerico/TP3")
source('utils/Euler-Mac Laurin.R')

'
li=Limite superior de la integral (ACEPTA NO ENTEROS)
ls=limite inferior de la integral (ACEPTA NO ENTEROS)
ti=Limite superior de la transformaci?n en caso de l?mites no enteros en caso que no se quiera redondear
ts=limite inferior de la transformaci?n en caso de l?mites no enteros en caso que no se quiera redondear
u=Cant n?meros de bernoulli con un m?ximo de u=11,Por defecto u=3
'
# Definimos la función 
func = expression(exp(0.7*X)*(cos(5*X)+sin(5*X)))
f = function(X){exp(0.7*X)*(cos(5*X)+sin(5*X))}

# Limites
li = 1
ls = 3

# Valor real
integral = integrate(f, li,ls)[1]
# La aproximación deberia dar 2.762193762069276
em_u3 = eulermaclaurint(li,ls,u=3)
em_u4 = eulermaclaurint(li,ls,u=4)
em_u5 = eulermaclaurint(li,ls,u=5)
em_u6 = eulermaclaurint(li,ls,u=6)
em_u7 = eulermaclaurint(li,ls,u=7)

# Error relativo
calcular_e_relativo = function(r,a){
  error_relativo = abs((r - a))/(r)
  return(error_relativo)
}

# Guardamos resultados en una tabla 
resultados = c(em_u3,em_u4,em_u5,em_u6, em_u7)
mus = c('u=3','u=4', 'u=5', 'u=6','u=7')
err = c(calcular_e_relativo(as.numeric(integral), em_u3),calcular_e_relativo(as.numeric(integral), em_u4),calcular_e_relativo(as.numeric(integral), em_u5),calcular_e_relativo(as.numeric(integral), em_u6),calcular_e_relativo(as.numeric(integral), em_u7))
tabla_resultados = data.frame(mus, resultados, err)
names(tabla_resultados) = c('u','aproximación', 'error_relativo')
tabla_resultados

#for(i in 1:11){
#  print(i)
#  print(eulermaclaurint(li,ls,u=i))}
'
Cambiamos de variables (cuadratura)
Trasnformación
t = a + b*x
dt = b*dx
'
lista = c(1,12,48,120)
# Resulevo el sistema de ecuaciones
coeficientes = c()
for(i in lista){
  linf = 0
  lsup = i
  data = c(1,linf,1,lsup)
  A = matrix(ncol=2, byrow=TRUE,data=data)
  b = matrix(c(1,3))
  coeficientes = c(coeficientes, solve(A,b))
}
# Tabla con coeficientes de la transformacion
tabla_coeficientes = matrix(data=coeficientes, ncol=2, byrow=TRUE)

# Limites superiores


# Aproximo luego de aplicar transformacion
limites = c('(0;1)', '(0;12)', '(0;48)','(0;120)')
aproximaciones = c()
valores_reales = c()
errores = c()
for(i in 1:4){
  # Calcula para cada transformacion la aproximacion y sus errores
  a = tabla_coeficientes[i,1]
  b = tabla_coeficientes[i,2]
  func = expression(exp(0.7*(a+b*X))*(cos(5*(a+b*X))+sin(5*(a+b*X))))
  f = function(x){exp(0.7*(a+b*x))*(cos(5*(a+b*x))+sin(5*(a+b*x)))}
  lsup = lista[i]
  linf = 0
  em = eulermaclaurint(linf, lsup,u=3)
  aproximaciones = c(aproximaciones, em)
  val = integrate(f, linf, lsup)
  valores_reales = c(valores_reales, as.numeric(val[1]))
  error = calcular_e_relativo(as.numeric(val[1]),em)
  errores = c(errores, error)
}

# Creo tabla con resultados
tabla_resultados2 = data.frame(limites, valores_reales, aproximaciones, errores)
names(tabla_resultados2) = c('Cuadratura', 'Valor real', 'Aproximación', 'Error Relativo')

tabla_resultados2




# 2. Trapecio, Simpson, Simpson 3/8

source('utils/Trapecio.R')

'
Trapecio
Se crea la funcion "trapecio" con argumentos:
a: Limite Inferior de la integral
b: Limite Superior de la integral
n: Tamaño de cada intervalo
'


## Aproximamos con la formula del trapecio con n=6
func = function(X){exp(0.7*X)*(cos(5*X)+sin(5*X))}
funci = expression(exp(0.7*X)*(cos(5*X)+sin(5*X)))
n = 6
trapecio_n6 = trapecio(1,3,n)
valor_real_integral = as.numeric(integrate(func, 1,3)[1])

# Calcular cota


# Derivada 2
calculate_fii = function(expr,var){
  'Recibe expresion y devuelve la 2da derivada'
  fii = D(D(expr,var),var)
  return(fii)
}

# Derivada 3 
calculate_fiii = function(expr,var){
  'Recibe expresion y devuelve la 2da derivada'
  fiii = D(D(D(expr,var),var), var)
  return(fiii)
}

# Derivada 4
calculate_fiv = function(expr,var){
  'Recibe una expresion y devuelve la derivada 4ta'
  fiv = D(D(D(D(expr,var),var),var),var)
  return(fiv)}

# Derivada 5
calculate_fv = function(expr,var){
  'Recibe una expresion y devuelve la derivada 4ta'
  fv = D(D(D(D(D(expr,var),var),var),var), var)
  return(fv)}

# Calculamos la cota de la formula de Trapecio
"
Cota de Trapecio
((h^3)/12)*n*fii(∂)
"
n = 6
h = 1/3

# Chequeo que valor mayora 
mayorandos = c()
for(phi in seq(1,3,1/3)){
  fii = calculate_fii(funci, 'X')
  X = phi
  mayorandos = c(mayorandos,phi, eval(fii))
}

matrix(data=mayorandos, ncol=2, byrow=TRUE)

# Phi que mayora entre 1 y 3 con h=1/3
phi = 2.6666666666666665186 # Chequear si este es el valor que mayora 

# Calculo la cota del trapecio
calculate_cota_trapecio = function(expr,h,n,phi){
  "Recibe la función, el 'h', el 'n', y el 'phi' que mayora"
  fii = calculate_fii(expr, 'X')
  X = phi
  eval_fii = eval(fii)
  cota_trapecio = expression(((h^3)/12)*n*abs(eval_fii))
  return(eval(cota_trapecio))}

cota_trapecio = calculate_cota_trapecio(funci, h,n, phi)


# Verifico que el resultado real este dentro de la aproximación
cota_trapecio


## Aproximamos con la formula de Simpson
source('utils/Simpson.R')

"
Se crea la funcion 'simpson' con argumentos:
a: Limite Inferior de la integral
b: Limite Superior de la integral
n: Tamaño de cada intervalo
"

func = function(X){exp(0.7*X)*(cos(5*X)+sin(5*X))}
simpson_n6 = simpson(1,3,6)


"
Cota de Simpson
((h^5)/90)*((n/2) * fiv(∂))
"

fiv_prueba = calculate_fiv(funci, 'X')

mayorandos = c()
for(i in seq(1,3,1/3)){
  X = i
  mayorandos = c(mayorandos, i, eval(fiv_prueba))
}

matrix(data=mayorandos, byrow = TRUE, ncol = 2)



# Calculamos la cota de simpson
phi = 2.6666666666666665186

calculate_cota_simpson = function(expr, h, n, phi){
  "Recibe una funcion, el 'h', el 'n', y el 'phi' que mayora"
  fiv = calculate_fiv(funci, 'X')
  X = phi
  eval_fiv = eval(fiv)
  cota_simpson = expression(((h^5)/90)*((n/2) * eval_fiv))
  return(eval(cota_simpson))
}

cota_simpson = calculate_cota_simpson(funci, h, n, phi)

# Verifico que el valor real se encuentre dentro de la aproximacion 
#(DA MAL VERIFICAR)
as.numeric(integral[1]) - simpson_n6 < cota_simpson


## Calculo con simpson 3/8
source('utils/Simpson3-8.R')
"
Se crea la funci?n 'simpson3_8' con argumentos:
a: Limite Inferior de la integral
b: Limite Superior de la integral
n: Tamaño de cada intervalo
"
# Aproximo con simpson38
func = function(X){exp(0.7*X)*(cos(5*X)+sin(5*X))}
simpson38_n6 = SIMPSON3_8(1,3,6)

# Calculo la cota de simpson38
"
Cota Simpson38
((3*(h^5))/80)((n/3)fiv(∂))
"
# Vemos que valor mayora
fiv_prueba = calculate_fiv(funci, 'X')
mayorandos = c()
for(i in seq(1,3,1/3)){
  X = i
  mayorandos = c(mayorandos, i, eval(fiv_prueba))
}
matrix(data=mayorandos, byrow = TRUE, ncol = 2)

# Calculamos la cota (chequear valor que mayora)
phi = 2.6666666666666665186

calculate_cota_simpson38 = function(expr, h, n, phi){
  "Recibe una funcion, el 'h', el 'n', y el 'phi' que mayora"
  fiv = calculate_fiv(funci, 'X')
  X = phi
  eval_fiv = eval(fiv)
  cota_simpson38 = expression(((3*(h^5))/80)*((n/3)*eval_fiv))
  return(eval(cota_simpson38))
}

cota_simpson38 = calculate_cota_simpson38(funci, h, n, phi)

# Verifico que el valor real se encuentre dentro de la aproximación y la cota 
#(CHEQUEAR)
as.numeric(integral[1]) - simpson38_n6  < cota_simpson38



# Repetimos los anteriores pasos pero utilzando n=12;n=48;n=120
# Si cambio el n cambia el h, si cambia el h cambia el phi?

resultados_trapecio = c()
resultados_simpson = c()
resultados_simpson38 = c()

enes = c(6,12,48,120)



for(n in enes){
  # Determiinamos el h
  h = (3-1)/n
  
  # Primero Trapecios
  # Mayoramos la derivada2
  mayorandos = c()
  for(phi in seq(1,3,h)){
    "Hace una tabla con la derivada2 evaluada para luego encontrar cual es el que mayora"
    fiii = calculate_fiii(funci, 'X')
    X = phi
    mayorandos = c(mayorandos,phi, abs(eval(fii)))
  }
  df_mayorandos = data.frame(matrix(data=mayorandos, byrow = TRUE, ncol=2))
  # Phi que mayora derivada2
  phi = df_mayorandos[1][df_mayorandos[,2] == max(df_mayorandos),]
  # Aproximación con trapecio
  aprox_trapecio = trapecio(1,3,n)
  cota_trap = calculate_cota_trapecio(funci, h, n, phi)
  error_trap = calcular_e_relativo(as.numeric(integral[1]), aprox_trapecio)
  resultados_trapecio = c(resultados_trapecio,n, aprox_trapecio, cota_trap, error_trap)
  
  # Segundo Simpson
  # Mayoramos la derivada4
  mayorandos = c()
  for(phi in seq(1,3,h)){
    "Datos para la tabla con la derivada4 evaluada y luego ver cual mayora"
    fiv = calculate_fiv(funci,'X')
    X = phi
    mayorandos = c(mayorandos,phi, abs(eval(fiv)))
  }
  df_mayorandos = data.frame(matrix(data=mayorandos, byrow = TRUE, ncol=2))
  # Phi que mayora derivada 4ta
  phi = df_mayorandos[1][df_mayorandos[,2] == max(df_mayorandos),]
  
  # Aproximación con Simpson
  aprox_simpson = simpson(1,3,n)
  cota_simp = calculate_cota_simpson(funci, h, n, phi)
  error_simp = calcular_e_relativo(as.numeric(integral[1]), aprox_simpson)
  resultados_simpson = c(resultados_simpson, n, aprox_simpson, cota_simp, error_simp)
  
  # Tercero Simpson 3/8
  aprox_simpson38 = SIMPSON3_8(1,3,n)
  cota_simp38 = calculate_cota_simpson38(funci, h, n, phi)
  error_simp38 = calcular_e_relativo(as.numeric(integral[1]), aprox_simpson38)
  resultados_simpson38 = c(resultados_simpson38, n , aprox_simpson38, cota_simp38, error_simp38)
}

tabla_trapecio = data.frame(matrix(data=resultados_trapecio, ncol = 4, byrow = TRUE))
names(tabla_trapecio) = c('n', 'aproximación', 'cota', 'error')
tabla_simpson = data.frame(matrix(data=resultados_simpson, ncol = 4, byrow = TRUE))
names(tabla_simpson) = c('n', 'aproximación', 'cota', 'error')
tabla_simpson38 = data.frame(matrix(data=resultados_simpson38, ncol = 4, byrow = TRUE))
names(tabla_simpson38) = c('n', 'aproximación', 'cota', 'error')






"
Trabajo Practico #3 (Ejercicio 2 - Sumación)

"

library(readxl)
data = read_excel("/Users/mac/Facultad/Analisis Numerico/TP3/Datos Macroeconómicos Para TP 3.xls")

setwd("/Users/mac/Facultad/Analisis Numerico/TP3")
options(digits=20)


## Sumas exactas
data[5:36,9]
colSums(data[5:36,9])

iv_trimestres = data[data$Trimestre == 'IV',]
iv_trimestres




# Sumación numérica Lubbock
source("utils/Lubbock.R")

"Argumentos:
a=indice del primer punto. 
n=intervalos incluidos en la sumatoria.
m=subperiodos.
o=opcion, para f?rmula ?nicamente con diferencias o=1 y o=2 para f?rmula con diferencias y nablas.
"

lista_sumar = c('Consumo_Público_D', 'Consumo_Privado_D', 'Exportaciones_D')
handy = c('Consumo Publico', 'Consumo Privado', 'Exportaciones')
reales = c(2287937.9490792425349,13718985.076737668365 ,4702028.0162050882354)
"
Consumo Publico: 2287938
Consumo Privado: 13718985
Exportaciones : 4702028
"

# Calculo lubbock delta y nabla para las 3 categorias
resultados_deltas = c()
resultados_nablas = c()
for(i in lista_sumar){  
  x = seq(4,64,4)
  A = cbind(x, iv_trimestres[i])
  n = 8
  m = 4
  lubbock_deltas = Lubbock(A,1,n,m,1)
  lubbock_deltas = lubbock_deltas + A[9,2] - A[1,2] # Sumo dato IV trimestre 2012, resto IV trimestre 2004
  lubbock_nablas = Lubbock(A,1,n,m,2)
  lubbock_nablas = lubbock_nablas + A[9,2] - A[1,2] # Sumo dato IV trimestre 2012, resto IV trimestre 2004
  resultados_deltas = c(resultados_deltas, lubbock_deltas)
  resultados_nablas = c(resultados_nablas, lubbock_nablas)
}

# Creo la matriz con resultados
m = matrix(data=c(handy, reales, resultados_deltas, resultados_nablas), ncol = 4)
df_lubbock = data.frame(m)
names(df_lubbock) = c('Categoria', 'ValorReal', 'Deltas', 'Nablas')

# Calculo el error relativo para cada aproximacion
calcular_e_relativo = function(r,a){
  error_relativo = abs((r - a))/(r)
  return(error_relativo)
}

df_lubbock['Error_relativ_deltas'] = calcular_e_relativo(reales, resultados_deltas)
df_lubbock['Error_relativ_nablas'] = calcular_e_relativo(reales, resultados_nablas)



# Interpolación polinomio grado 8 

source('utils/Newton TP2.R')
source('utils/Woolhouse.R')
setwd("/Users/mac/Facultad/Analisis Numerico/TP3")
source('utils/Diferencias Divididas.R')

# Calculamos polinomio de newton grado 8
diff_divs = c()
polinomios_newton = c()
for(i in lista_sumar){
  iv_trimestres = data[data$Trimestre == 'IV',]
  x = seq(0,60,4)
  A = cbind(x, iv_trimestres[i])
  print('Diferencias divididas')
  diff = diferenciasdivididas(A[1:9,])
  diff_divs = c(diff_divs, diff)
  
  print(('Calculo productorias'))
  out = matrix(ncol=1, nrow=9)
  acumulado = 1
  for(i in 1:9){
    out[i,1] = acumulado
    val = paste('(x','-',A[i,1],')')
    acumulado = paste(acumulado,'*', val)
  }
  
  print('Calculo terminos del polinomio')
  terminos = matrix(nrow=9, ncol=1)
  for(i in 1:9){
    difdiv = as.numeric(diff[1,i+1])
    val = out[i]
    termino = paste('(',difdiv, '*', val,')')
    terminos[i] = termino
  }
  
  polinomios_newton = c(polinomios_newton, terminos)
}

# DataFrame con polinomio grado 8 para cada categoria 
newton = data.frame(matrix(data=polinomios_newton, ncol=3))
names(newton) = lista_sumar


polinomios = c()
for(k in 1:3){
  acumulado = 0
  for(i in 1:9){
    X = newton[1:9, k]
    acumulado = paste(acumulado, '+',X[i])
  }
  polinomios = c(polinomios, acumulado)
  print(paste('Listo', names(newton)[k]))
}

# Polinomios guardados en string 
polinomio_Pub =  polinomios[1]
polinomio_Priv = polinomios[2]
polinomio_Expo = polinomios[3]

# Polinomios en expression
polinomio_Pub = expression(0 + ( 55747.3255050337 * 1 ) + ( 1231.86171963932 * 1 * (x - 0 ) ) + ( -85.4468409297249 * 1 * (x - 0 ) * (x - 4 ) ) + ( 13.5702232614922 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) ) + ( -1.57108519636788 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) ) + ( 0.158672242624122 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) * (x - 16 ) ) + ( -0.0137532925982311 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) * (x - 16 ) * (x - 20 ) ) + ( 0.000953403153829165 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) * (x - 16 ) * (x - 20 ) * (x - 24 ) ) + ( -5.14741579658443e-05 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) * (x - 16 ) * (x - 20 ) * (x - 24 ) * (x - 28 ) ))
polinomio_Priv = expression(0 + ( 324623.597322858 * 1 ) + ( 4822.55302720526 * 1 * (x - 0 ) ) + ( 543.851762337374 * 1 * (x - 0 ) * (x - 4 ) ) + ( -40.1558665089087 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) ) + ( -2.49689790113315 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) ) + ( 0.452321771466212 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) * (x - 16 ) ) + ( -0.00636982902164621 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) * (x - 16 ) * (x - 20 ) ) + ( -0.00329988608325539 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) * (x - 16 ) * (x - 20 ) * (x - 24 ) ) + ( 0.000410727024119009 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) * (x - 16 ) * (x - 20 ) * (x - 24 ) * (x - 28 ) ))
polinomio_Expo = expression(0 + ( 119993.995164765 * 1 ) + ( 2100.811005459 * 1 * (x - 0 ) ) + ( 203.721617318437 * 1 * (x - 0 ) * (x - 4 ) ) + ( -15.1343011529192 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) ) + ( -4.09612779962566 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) ) + ( 0.768087746060459 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) * (x - 16 ) ) + ( -0.0634646341337326 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) * (x - 16 ) * (x - 20 ) ) + ( 0.00317521412256873 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) * (x - 16 ) * (x - 20 ) * (x - 24 ) ) + ( -0.000101969986854068 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) * (x - 16 ) * (x - 20 ) * (x - 24 ) * (x - 28 ) ))


# Woolhouse
"
Se crea la funcion woolhouse con argumentos:
n: Cantidad de periodos
m: Cantidad de subperiodos
u: Cantidad de terminos de Bernoulli a utilizar.Por defecto u=3.Maximo u=11
"
"
Consumo Publico: 2287938
Consumo Privado: 13718985
Exportaciones : 4702028
"

# Consumo Privado 
func=polinomio_Priv
m = seq(0,32,4)
n = c()
for(i in m){
  x = i
  n = c(n, eval(func))
}
c<-c(1:length(m))
Datos<-cbind(c,m,n)
w_privado = Woolhouse(Datos,1,8,4,u=3)

# Consumo Publico 
func=polinomio_Pub
m = seq(0,32,4)
n = c()
for(i in m){
  x = i
  n = c(n, eval(func))
}
c<-c(1:length(m))
Datos<-cbind(c,m,n)
w_publico = Woolhouse(Datos,1,8,4,u=3)

# Exportaciones
func=polinomio_Expo
m = seq(0,32,4)
n = c()
for(i in m){
  x = i
  n = c(n, eval(func))
}
c<-c(1:length(m))
Datos<-cbind(c,m,n)
w_expo = Woolhouse(Datos,1,8,4,u=3)

# Resultados de Woolhouse
df_woolhouse = data.frame(indice=c('Valor aproximado', 'Valor Real', 'Error Relativo'), 
                          w_publico=c(w_publico,2287938, calcular_e_relativo(2287938, w_publico)), 
                          w_privado=c(w_privado,13718985, calcular_e_relativo(13718985,w_privado)), 
                          w_expo=c(w_expo, 4702028, calcular_e_relativo(4702028, w_expo))
)

# Suma de interpolacion Newton


indices = c(6,7,9)
n_interpolados = c()
errores = c()
for(i in indices){
  # Selecciono la data a usar 
  new_data = data[4:36,]
  x = seq(1,33,4)
  total = seq(1,33)
  xs = total[!total %in% x]
  fx = new_data[new_data$Trimestre == 'IV', i]
  
  A = cbind(x, fx)
  n_interpolados = c(n_interpolados,sum(fx[2:9,]), Newton(A, xs))
}

# Resultados sumas newton

newton_interpolacion = data.frame(matrix(data=n_interpolados, ncol=3))
sumas_interpolacion_n = colSums(newton_interpolacion)
#names(suma_interpolacino_n) = c('Consumo Privado', 'Consumo Publico', 'Exportaciones')
calcular_e_relativo(13718985.0767376683652,sumas_interpolacion_n[1])
calcular_e_relativo(2287937.9490792425349,sumas_interpolacion_n[2])
calcular_e_relativo(4702028.0162050882354,sumas_interpolacion_n[3])

# Sumatoria de resultados Splines


source('utils/Splines - frontera libre para TP 2.R')

indices = c(6,7,9)
s_interpolados = c()
for(i in indices){
  # Selecciono la data a usar 
  new_data = data[4:36,]
  x = seq(1,33,4)
  total = seq(1,33)
  xs = total[!total %in% x]
  fx = new_data[new_data$Trimestre == 'IV', i]
  
  A = cbind(x, fx)
  s_interpolados = c(s_interpolados,sum(fx[2:9,]), spline_LG(A, xs))
}

spline_interpolacion = data.frame(matrix(data=s_interpolados, ncol=3))
sumas_interpolacion_s = colSums(spline_interpolacion)
calcular_e_relativo(13718985.0767376683652,sumas_interpolacion_s[1])
calcular_e_relativo(2287937.9490792425349,sumas_interpolacion_s[2])
calcular_e_relativo(4702028.0162050882354,sumas_interpolacion_s[3])




# Dividir la serie en 2 

df1 = data[4:20,c(2,6,7,9)]
df2 = data[20:36,c(2,6,7,9)]


# 1. Lubbock
lista_sumar = c('Consumo_Público_D', 'Consumo_Privado_D', 'Exportaciones_D')
handy = c('Consumo Publico', 'Consumo Privado', 'Exportaciones')
reales = c(2287937.9490792425349,13718985.076737668365 ,4702028.0162050882354)

# Guardamos resultados
resultados_deltas1 = c()
resultados_nablas1 = c()
resultados_deltas2 = c()
resultados_nablas2 = c()
# Hacemos lubbock para el df dividido
for(i in lista_sumar){  
  x = seq(0,60,4)
  A = cbind(x, iv_trimestres[i])
  n = 4
  m = 4
  # Parte1
  lubbock_deltas1 = Lubbock(A,1,n,m,1)
  lubbock_deltas1 = lubbock_deltas1  - A[1,2] # Resto IV trimestre 2004
  lubbock_nablas1 = Lubbock(A,1,n,m,2)
  lubbock_nablas1 = lubbock_nablas1  - A[1,2] # Resto IV trimestre 2004
  resultados_deltas1 = c(resultados_deltas1, lubbock_deltas1)
  resultados_nablas1 = c(resultados_nablas1, lubbock_nablas1)
  # Parte2
  lubbock_deltas2 = Lubbock(A,5,n,m,1)
  lubbock_deltas2 = lubbock_deltas2 + A[9,2]  # Sumo dato IV trimestre 2012
  lubbock_nablas2 = Lubbock(A,5,n,m,2)
  lubbock_nablas2 = lubbock_nablas2 + A[9,2]  # Sumo dato IV trimestre 2012
  resultados_deltas2 = c(resultados_deltas2, lubbock_deltas2)
  resultados_nablas2 = c(resultados_nablas2, lubbock_nablas2)
}

# Guardamos en un df los resultados
deltas_sum = resultados_deltas2 + resultados_deltas1
nablas_sum = resultados_nablas1 + resultados_nablas2
m = matrix(data=c(handy, reales, deltas_sum, nablas_sum), ncol = 4)
df_lubbock_dividido = data.frame(m)
names(df_lubbock_dividido) = c('Categoria', 'ValorReal', 'Deltas', 'Nablas')

df_lubbock_dividido['Error_relativ_deltas'] = calcular_e_relativo(reales, deltas_sum)
df_lubbock_dividido['Error_relativ_nablas'] = calcular_e_relativo(reales, nablas_sum)


# 2. Woolhouse
# Calculamos primeros polinomios de newton grado 4
diff_divs = c()
polinomios_newton1 = c()
for(i in lista_sumar){
  iv_trimestres = df1[df1$Trimestre == 'IV',]
  x = seq(0,16,4)
  A = cbind(x, iv_trimestres[,i])
  print('Diferencias divididas')
  diff = diferenciasdivididas(A[1:5,])
  diff_divs = c(diff_divs, diff)
  
  print(('Calculo productorias'))
  out = matrix(ncol=1, nrow=5)
  acumulado = 1
  for(i in 1:5){
    out[i,1] = acumulado
    val = paste('(x','-',A[i,1],')')
    acumulado = paste(acumulado,'*', val)
  }
  
  print('Calculo terminos del polinomio')
  terminos = matrix(nrow=5, ncol=1)
  for(i in 1:5){
    difdiv = as.numeric(diff[1,i+1])
    val = out[i]
    termino = paste('(',difdiv, '*', val,')')
    terminos[i] = termino
  }
  
  polinomios_newton1 = c(polinomios_newton1, terminos)
}
# DataFrame1 con polinomio grado 4 para cada categoria 
newton1 = data.frame(matrix(data=polinomios_newton1, ncol=3))
names(newton1) = lista_sumar

# Calculams segundos polinomios de newton grado 4
diff_divs = c()
polinomios_newton2 = c()
for(i in lista_sumar){
  iv_trimestres = df2[df2$Trimestre == 'IV',]
  x = seq(0,16,4)
  A = cbind(x, iv_trimestres[,i])
  print('Diferencias divididas')
  diff = diferenciasdivididas(A[1:5,])
  diff_divs = c(diff_divs, diff)
  
  print(('Calculo productorias'))
  out = matrix(ncol=1, nrow=5)
  acumulado = 1
  for(i in 1:5){
    out[i,1] = acumulado
    val = paste('(x','-',A[i,1],')')
    acumulado = paste(acumulado,'*', val)
  }
  
  print('Calculo terminos del polinomio')
  terminos = matrix(nrow=5, ncol=1)
  for(i in 1:5){
    difdiv = as.numeric(diff[1,i+1])
    val = out[i]
    termino = paste('(',difdiv, '*', val,')')
    terminos[i] = termino
  }
  
  polinomios_newton2 = c(polinomios_newton2, terminos)
}
# DataFrame1 con polinomio grado 4 para cada categoria 
newton2 = data.frame(matrix(data=polinomios_newton2, ncol=3))
names(newton2) = lista_sumar


# Guardamos los polinomios de grado 4 
polinomios1 = c()
for(k in 1:3){
  acumulado = 0
  for(i in 1:5){
    X = newton1[1:5, k]
    acumulado = paste(acumulado, '+',X[i])
  }
  polinomios1 = c(polinomios1, acumulado)
  print(paste('Listo', names(newton1)[k]))
}
polinomios2 = c()
for(k in 1:3){
  acumulado = 0
  for(i in 1:5){
    X = newton2[1:5, k]
    acumulado = paste(acumulado, '+',X[i])
  }
  polinomios2 = c(polinomios2, acumulado)
  print(paste('Listo', names(newton2)[k]))
}

# Polinomios grado 4
publico_1 = expression(0 + ( 55747.3255050337 * 1 ) + ( 1231.86171963932 * 1 * (x - 0 ) ) + ( -85.4468409297249 * 1 * (x - 0 ) * (x - 4 ) ) + ( 13.5702232614922 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) ) + ( -1.57108519636788 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) ))
privado_1 = expression(0 + ( 324623.597322858 * 1 ) + ( 4822.55302720526 * 1 * (x - 0 ) ) + ( 543.851762337374 * 1 * (x - 0 ) * (x - 4 ) ) + ( -40.1558665089087 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) ) + ( -2.49689790113315 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) ))
expo_1 = expression(0 + ( 119993.995164765 * 1 ) + ( 2100.811005459 * 1 * (x - 0 ) ) + ( 203.721617318437 * 1 * (x - 0 ) * (x - 4 ) ) + ( -15.1343011529192 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) ) + ( -4.09612779962566 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) ))

publico_2 = expression(0 + ( 70242.4350439234 * 1 ) + ( 1535.67525543638 * 1 * (x - 0 ) ) + ( -74.2640697403663 * 1 * (x - 0 ) * (x - 4 ) ) + ( 0.190142114025358 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) ) + ( 0.630159222561839 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) ))
privado_2 = expression(0 + ( 429183.632464672 * 1 ) + ( -2349.58557225025 * 1 * (x - 0 ) ) + ( 2100.56275489047 * 1 * (x - 0 ) * (x - 4 ) ) + ( -236.788181854513 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) ) + ( 14.5873389311164 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) ))
expo_2 = expression(0 + ( 144308.626005465 * 1 ) + ( -1668.40834635525 * 1 * (x - 0 ) ) + ( 707.463034533405 * 1 * (x - 0 ) * (x - 4 ) ) + ( -69.393523563966 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) ) + ( 1.41700486315835 * 1 * (x - 0 ) * (x - 4 ) * (x - 8 ) * (x - 12 ) ))

mat1 = matrix(data=c(publico_1,privado_1, expo_1))
mat2 = matrix(data=c(publico_2,privado_2, expo_2))



# Consumo Publico 
# Primera mitad
func= publico_1
m = seq(0,16,4)
n = c()
for(i in m){
  x = i
  n = c(n, eval(func))
}
c<-c(1:length(m))
Datos<-cbind(c,m,n)
pub1 = Woolhouse(Datos,1,4,4,u=3)
# Segunda mitad
func = publico_2
m = seq(0,16,4)
n = c()
for(i in m){
  x = i
  n = c(n, eval(func))
}
c<-c(1:length(m))
Datos<-cbind(c,m,n)
pub2 = Woolhouse(Datos,1,4,4,u=3)

# Total 
w_publico_div = pub1 + pub2

# Consumo Privado 
# Primera mitad
func= privado_1
m = seq(0,16,4)
n = c()
for(i in m){
  x = i
  n = c(n, eval(func))
}
c<-c(1:length(m))
Datos<-cbind(c,m,n)
priv1 = Woolhouse(Datos,1,4,4,u=3)
# Segunda mitad
func = privado_2
m = seq(0,16,4)
n = c()
for(i in m){
  x = i
  n = c(n, eval(func))
}
c<-c(1:length(m))
Datos<-cbind(c,m,n)
priv2 = Woolhouse(Datos,1,4,4,u=3)

# Total 
w_privado_div = priv1 + priv2

# Exportaciones 
# Primera mitad
func= expo_1
m = seq(0,16,4)
n = c()
for(i in m){
  x = i
  n = c(n, eval(func))
}
c<-c(1:length(m))
Datos<-cbind(c,m,n)
exp1 = Woolhouse(Datos,1,4,4,u=3)
# Segunda mitad
func = expo_2
m = seq(0,16,4)
n = c()
for(i in m){
  x = i
  n = c(n, eval(func))
}
c<-c(1:length(m))
Datos<-cbind(c,m,n)
exp2 = Woolhouse(Datos,1,4,4,u=3)

# Total 
w_expo_div = exp1 + exp2

# Resultados de Woolhouse
df_woolhouse_dividido = data.frame(indice=c('Valor aproximado', 'Valor Real', 'Error Relativo'), 
                                   publico=c(w_publico_div,2287938, calcular_e_relativo(2287938, w_publico_div)), 
                                   privado=c(w_privado_div,13718985, calcular_e_relativo(13718985,w_privado_div)), 
                                   expo=c(w_expo_div, 4702028, calcular_e_relativo(4702028, w_expo_div)))






# 3. Newton

indices = c(2,3,4)
n1_interpolados = c()
n2_interpolados = c()
for(i in indices){
  # Newton1
  data1 = df1[1:17,]
  
  x1 = seq(0,16,4)
  total = seq(0,16)
  xs1 = total[!total %in% x1]
  fx1 = data1[data1$Trimestre == 'IV', i]
  A1 = cbind(x1, fx1)
  n1_interpolados = c(n1_interpolados,sum(fx1[2:5,]), Newton(A1, xs1))
  print('Parte 1 listo!')
  # Newton27
  data2 = df2[1:17,]
  total = seq(16,32)
  x2 = seq(16,32,4)
  xs2 = total[!total %in% x2]
  fx2 = data2[data2$Trimestre == 'IV', i]
  A2 = cbind(x2, fx2)
  n2_interpolados = c(n2_interpolados,sum(fx2[2:5,]), Newton(A2, xs2))
  print('Parte 2 listo!')
  
  
}

# Resultados sumas newton
i1 = matrix(n1_interpolados,ncol=3)
i2 = matrix(n2_interpolados,ncol=3)
newton_interpolacion_div = colSums(i1) + colSums(i2)
sumas_interpolacion_n_div = data.frame(matrix(newton_interpolacion_div, ncol=3))
names(sumas_interpolacion_n_div) = c('Consumo Privado', 'Consumo Publico', 'Exportaciones')

calcular_e_relativo(13718985.0767376683652,as.numeric(sumas_interpolacion_n_div[1]))
calcular_e_relativo(2287937.9490792425349,as.numeric(sumas_interpolacion_n_div[2]))
calcular_e_relativo(4702028.0162050882354,as.numeric(sumas_interpolacion_n_div[3]))


# Spline dividido

indices = c(2,3,4)
s1_interpolados = c()
s2_interpolados = c()
for(i in indices){
  # Newton1
  data1 = df1[1:17,]
  
  x1 = seq(0,16,4)
  total = seq(0,16)
  xs1 = total[!total %in% x1]
  fx1 = data1[data1$Trimestre == 'IV', i]
  A1 = cbind(x1, fx1)
  s1_interpolados = c(s1_interpolados,sum(fx1[2:5,]), spline_LG(A1, xs1))
  print('Parte 1 listo!')
  # Newton27
  data2 = df2[1:17,]
  total = seq(16,32)
  x2 = seq(16,32,4)
  xs2 = total[!total %in% x2]
  fx2 = data2[data2$Trimestre == 'IV', i]
  A2 = cbind(x2, fx2)
  s2_interpolados = c(s2_interpolados,sum(fx2[2:5,]), spline_LG(A2, xs2))
  print('Parte 2 listo!')
  
}


# Resultados sumas spline
i1 = matrix(s1_interpolados,ncol=3)
i2 = matrix(s2_interpolados,ncol=3)
spline_interpolacion_div = colSums(i1) + colSums(i2)
sumas_interpolacion_s_div = data.frame(matrix(spline_interpolacion_div, ncol=3))
names(sumas_interpolacion_s_div) = c('Consumo Privado', 'Consumo Publico', 'Exportaciones')


calcular_e_relativo(13718985.0767376683652,as.numeric(sumas_interpolacion_s_div[1]))
calcular_e_relativo(2287937.9490792425349,as.numeric(sumas_interpolacion_s_div[2]))
calcular_e_relativo(4702028.0162050882354,as.numeric(sumas_interpolacion_s_div[3]))

df1
cac = c()
for(i in df1[,2]){cac = c(cac, i)}


priv =c()
for(i in iv_trimestres[,6]){priv = c(priv,i)}
vol_priv = c()
for(i in 2:16){
  vol = (priv[i] - priv[i-1]) / priv[i-1]
  vol_priv = c(vol_priv, vol)
}



pub = c()
for(i in iv_trimestres[,7]){pub = c(pub,i)}
vol_pub = c()
for(i in 2:16){
  vol = (pub[i] - pub[i-1]) / pub[i-1]
  vol_pub = c(vol_pub, vol)
}


exp = c()
for(i in iv_trimestres[,9]){exp = c(exp,i)}
vol_exp = c()
for(i in 2:16){
  vol = (exp[i] - exp[i-1]) / exp[i-1]
  vol_exp = c(vol_exp, vol)
}

volatilidades = matrix(data=c(vol_priv, vol_pub, vol_exp), ncol = 3)
matplot(volatilidades, type=c('l'))
title('Volatilidad de las series')
legend("topleft", legend = c('vol_priv','vol_pub', 'vol_exp'), col=1:3, pch='l')




