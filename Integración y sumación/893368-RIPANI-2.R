'
Trabajo práctico #2
Análisis Numérico
Máximo Ripani
Facultad de Ciencias Económicas de Buenos Aires
'


# 1. Integración

options(digits=20)

setwd("/Users/mac/Facultad/Analisis Numerico/TP3")
source('utils/Euler-Mac Laurin.R')


# Función 
func = expression(exp(0.7*X)*(cos(5*X)+sin(5*X)))
f = function(X){exp(0.7*X)*(cos(5*X)+sin(5*X))}

# Limites
li = 1
ls = 3

# Valor real
integral = integrate(f, li,ls)[1]

# Aproximaciones
# u3 1.3341547906356678865
em_u3 = eulermaclaurint(li,ls,u=3)
# u4 2.4690703323751064246
em_u4 = eulermaclaurint(li,ls,u=4)
# u5 2.9871984191830671662
em_u5 = eulermaclaurint(li,ls,u=5)
# u6 3.163047797359653579
em_u6 = eulermaclaurint(li,ls,u=6)
# u7 3.1660575547591909285
em_u7 = eulermaclaurint(li,ls,u=7)

# Error real
calcular_e = function(r,a){
  error = r-a
  return(error)
}


# Errores
err1 = calcular_e(as.numeric(integral[1]), em_u3) # 1.4280389714336103602
err2 = calcular_e(as.numeric(integral[1]), em_u4) # 0.29312342969417182204
err3 = calcular_e(as.numeric(integral[1]), em_u5) # -0.22500465711378891953
err4 = calcular_e(as.numeric(integral[1]), em_u6) # -0.40085403529037533232
err5 = calcular_e(as.numeric(integral[1]), em_u7) # -0.40386379268991268177

# Cuadratura
'
Trasnformación
t = a + b*x
dt = b*dx
'
#Cuadratura limites [0;1]  57.102970185848939
func=expression(((exp(1.4*X+0.7))*(cos(10*X+5)+sin(10*X+5)))*2) 
calcular_e(as.numeric(integral), 57.102970185848939)


#Cuadratura limites [0;12] 2.7621691255017065
func=expression(((exp((7/60)*X+0.7))*(cos((5/6)*X+5)+sin((5/6)*X+5)))*1/6) 
calcular_e(as.numeric(integral), 2.7621691255017065)

#Cuadratura limites [0;48] 2.7621937561258014
func=expression(((exp((7/240)*X+0.7))*(cos((5/24)*X+5)+sin((5/24)*X+5)))*1/24) 
calcular_e(as.numeric(integral), 2.7621937561258014)

#Cuadratura limites [0;120] 2.7621937620449484
func=expression(((exp((7/600)*X+0.7))*(cos((5/60)*X+5)+sin((5/60)*X+5)))*1/60) 
calcular_e(as.numeric(integral), 2.7621937620449484)


# 2. Trapecio, Simpson, Simpson 3/8
source('utils/Trapecio.R')

## Aproximamos con la formula del trapecio con n=6
func = function(X){exp(0.7*X)*(cos(5*X)+sin(5*X))}
funci = expression(exp(0.7*X)*(cos(5*X)+sin(5*X)))
n = 6

valor_real_integral = as.numeric(integrate(func, 1,3)[1])

# n6 2.0868141398644048934
trapecio_n6 = trapecio(1,3,n)
# n12 2.5989573431540122783
trapecio_n12 = trapecio(1,3,12)
# n48 2.7520950399613579584
trapecio_n48 = trapecio(1,3,48)
# n120 2.7605788815599177077
trapecio_n120 = trapecio(1,3,120)


# Errores
err1 = calcular_e(valor_real_integral, trapecio_n6) #0.6753796222048733533
err2 = calcular_e(valor_real_integral, trapecio_n12) #0.16323641891526686 
err3 = calcular_e(valor_real_integral, trapecio_n48) #0.010098722107920732 
err4 = calcular_e(valor_real_integral, trapecio_n120)#0.001614880509360539 

# Cotas de Trapecio
c1 = 4.5441399947688597
c2 = 1.1360349986922149
c3 = 0.071002187418263432 
c4 = 0.011360349986922149 


## Aproximamos con la formula de Simpson
source('utils/Simpson.R')

func = function(X){exp(0.7*X)*(cos(5*X)+sin(5*X))}

# n6 2.9122663212646391351
simpson_n6 = simpson(1,3,6)
# n12 2.7696717442505480733
simpson_n12 = simpson(1,3,12)
# n48 2.7622210866361736059
simpson_n48 = simpson(1,3,48)
# n120 2.7621944590141578324
simpson_n120 = simpson(1,3,120)


err1 = calcular_e(valor_real_integral, simpson_n6) #-0.15007255919536089 
err2 = calcular_e(valor_real_integral, simpson_n12) #-0.0074779821812693825 
err3 = calcular_e(valor_real_integral, simpson_n48) #-0.000027324566894915137
err4 = calcular_e(valor_real_integral, simpson_n120) #-0.000000696944879585714 

# Cotas simpson
c1 = -0.89207666143414766 
c2 = -0.055754791339634228 
c3 = -0.0002177921536704462 
c4 = -0.000005575479133951826 



## Calculo con simpson 3/8
source('/Users/mac/Facultad/Analisis Numerico/TP3/utils/Simpson3-8.R')

SIMPSON3_8<-function(a,b,n){
  
  if ((n/3)!=round((n/3))) return("n debe ser m?ltiplo de 3")
  else {
    h = (b-a)/n
    
    #Armo dos secuencias que combinadas sean todos lo no multiplos de 3    
    
    if(n>3) m1=seq(1,n-1,3) else m1=1 
    if (n>3) m2=seq(2,n-1,3) else m2=2
    
    # todos los numeros multiplos de 3 ,desde 1 hasta n-3  
    
    if (n>3) m3= seq(3, n-3, by = 3) else m3=0 
    
    #Sumatoria de todos los valores que toma la funcion en a+ih siendo i no m?ltiplo de 3    
    
    M1=sum(func(a+(h*m1)))      
    M2=sum(func(a+(h*m2)))   
    
    #Sumatoria de todos los valores que toma la funcion en a+ih siendo i  multiplo de 3
    
    if (n>3)M3=sum(func(a+(h*m3))) else M3=0 
    
    #Reemplazo en la funcion de Simpson.        
    
    integral= ((3/8)*h)*(func(a)+func(b)+(3*M1)+(2*M3)+(3*M2))
    
    return(integral)
  }
}

# Aproximo con simpson38
func = function(X){exp(0.7*X)*(cos(5*X)+sin(5*X))}

# n6 3.1895799634707606707
simpson38_n6 = SIMPSON3_8(1,3,6)
# n12 2.7803267912696059128
simpson38_n12 = SIMPSON3_8(1,3,12)
# n48 2.7622555125729713055
simpson38_n48 = SIMPSON3_8(1,3,48)
# n120 2.7621953312918381407
simpson38_n120= SIMPSON3_8(1,3,120)

# Errores
err1 = calcular_e(as.numeric(integral[1]), simpson38_n6) # -0.42738620140148242399
err2 = calcular_e(as.numeric(integral[1]), simpson38_n12) # -0.018133029200327666075
err3 = calcular_e(as.numeric(integral[1]), simpson38_n48) # -0.000061750503693058789
err4 = calcular_e(as.numeric(integral[1]), simpson38_n120) # -0.000001569222559894001

# Cotas
c1 = -0.42738620140148198 
c2 = -0.018133029200327666 
c3 = -0.000061750503693058789 
c4 = -0.000001569222559894001

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




