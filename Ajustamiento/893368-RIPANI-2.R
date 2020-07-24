# Análisis Numérico
# Trabajo Práctico 4
# Máximo Ripani
# Profesor: Julio Eduardo Fabris 





# Datos a verificar tabla Nocon y Scott 
x = c(1:21)
z=1
#h=?
m = rep(1,21)
u = c(9.5, 24.8, 19.8,  5.8, 10.3, 16.5, 27.5, 12.4, 35.6, 51.7, 26.3, 23.9, 39.9, 45.6, 45.9, 43.1, 47.4, 42.7, 40.2, 31.2, 44.5)
h1 = 0 
h2 = 97
h3 = 0




"
Función de Whittaker para z = 3
Toma los siguiente valores:
x = vector de valores equiespaciados
m = vector con la cantidad de observaciones de la que se obtuvo cada muestra
u = vector con valores observados
h = ponderadores de regularidad vs fidelidad
"

whit = function(x,m,u,h1,h2,h3){
  # Definimos n
  n = length(x)
  # Distancia entre los valores
  d = x[2] - x[1]
  a = seq(x[1],(x[1]+(n-1)*d),by=d)
  
  W = (diag(m))*(n/sum(m))
  # Calculamos las ki (numeros combinatorios)
  # z=1
  z = 1
  k1 = (choose(z,0:z))              
  for(i in 1:length(k1)){
    k1[i]=k1[i]*((-1)^i)
  }
  
  # z=2
  z = 2
  k2 = (choose(z,0:z))              
  for(i in 1:length(k2)){
    k2[i]=k2[i]*((-1)^i)
  }
  
  # z=3
  z = 3
  k3 = (choose(z,0:z))              
  for(i in 1:length(k3)){
    k3[i]=k3[i]*((-1)^i)
  }
  
  # Calculamos las matrices Ki (n-z)xn
  
  # K1 (21-1)x21 = 20x21
  z = 1
  K1 = diag(0,(n-z),n)
  for(i in 1:nrow(K1)){          
    for (j in 1:ncol(K1)){
      if(i==j){
        K1[i,(j:(j+z))]<-c(k1)
      }
    }
  }
  
  # K2 (21-2)x21 = 19x21
  z = 2
  K2 = diag(0,(n-z),n)
  for(i in 1:nrow(K2)){          
    for (j in 1:ncol(K2)){
      if(i==j){
        K2[i,(j:(j+z))]<-c(k2)
      }
    }
  }
  
  
  # K3 (21-3)x21 = 18x21
  z = 3
  K3 = diag(0,(n-z),n)
  for(i in 1:nrow(K3)){          
    for (j in 1:ncol(K3)){
      if(i==j){
        K3[i,(j:(j+z))]<-c(k3)
      }
    }
  }
  
  # Transpuestas
  TK1 = t(K1)
  TK2 = t(K2)
  TK3 = t(K3)
  
  
  # chequeamos determinante distinto de 0 
  # buscamos minimos vi
  C = solve(TK1%*%K1*h1+TK2%*%K2*h2+TK3%*%K3*h3+W)
  
  v = C %*% W %*% u
  colnames(v)<-c("vi")
  resultado<-data.frame(x,u,m,v)
  return(resultado)
}

# Aplicamos Whittaker
# Verificamos que funciones con la tabla de nocon y scott
tabla1_nocon_scott = whit(x,m,u,h1,h2,h3)

# Ahora con nuestros datos 
# Generamos los datos 
registro=893368 
set.seed(registro)
x=1:20
m=runif(20,5,15)
u=round(100+x/3+x^2/40-x^3/600+rnorm(20,0,2),1)

h1 = 1
h2 = 97
h3 = 1160


# Aplicamos Whittaker con nuestros datos 
tabla_resultados_1 = whit(x,m,u,h1,h2,h3)
tabla_resultados_1

write.csv(tabla_resultados_1, '/Users/mac/Facultad/Analisis Numerico/TP4/tabla_1.csv')


# Calculo RMSE
rmse = function(m, o){
  sqrt(mean((m - o)^2))
}

# Calculo RMSE con tabla result 1 
rmse(tabla_resultados_1$vi, tabla_resultados_1$u)


## 

"
Función de Whittaker para z = 1
"



calcular_GCV = function(x,u,m,v,W,K,TK,h,n){
  return((t(v-u)%*% W %*%(v-u))/(length(x)%*%((1-((sum(diag(solve(TK %*% K * h + W)%*%W))))/n)^2)))
}


# Whittaker para GCV
whittaker_GCV<-function(x,z,m,u){
  gcv = c()
  n<-length(x)
  d<-x[2]-x[1]
  a<-seq(x[1],(x[1]+(n-1)*d),by=d)
  
  
  for(i in 1:length(x)){
    if(x[i]==a[i])
      p=0 else p=1}
  if(p==0){
    # la funci?n procedera a verificar que z es menor que la 
    # longitud de x para que exista soluci?n
    if(z<n) {  
      # Crea la matriz W la cual tiene en la diagonal principal 
      # los valores de wx= mx/m.
      # Siendo m el promedio aritmetico de los tama?os de muestra.
      W<-(diag(m))*(n/sum(m))            
      # Calculamos la matriz k , conformada por los n?meros combinatorios
      # con signos alternados. #La funcion choose calcula dichos n?meros.                                        
      k<-(choose(z,0:z))              
      for(i in 1:length(k))           
        k[i]=k[i]*((-1)^i)
      # Dicha matriz tiene n columnas y (n-z) filas. En la posici?n
      # i=j de una matriz conformada por O se introducen los n?combinatorios.
      K<-diag(0,(n-z),n)            
      for(i in 1:nrow(K)){          
        for (j in 1:ncol(K)){
          if(i==j)  K[i,(j:(j+z))]<-c(k)
        }
      }
      TK<-t(K)                       #Traspuesta de la matriz K.
      
      for(i in 1:5000){
        # La funci?n proceder? a buscar la solucion , es decir los vi 
        # que minimizan la funci?n.
        h = i
        C<-solve(TK %*% K * h + W)             
        v<-C %*% W %*% u
        colnames(v)<-c("vi")
        Resultado<-data.frame(x,u,m,v)
        gcv = c(gcv,h, calcular_GCV(x,u,m,v,W,K,TK,h,n), rmse(v,u))
      }
      GCV = data.frame(matrix(data=gcv, ncol=3,byrow=TRUE))
      names(GCV)[1] = 'lambda'
      names(GCV)[2] = 'GCV'
      names(GCV)[3] = 'RMSE'
      return(GCV)
      
    }
    else print("La longitud de el vector x debe ser menor a z")
  }
  else print("Los argumentos de x no son equiespaciados")
}

# Verifico datos con tabla (lambda=97 para z=2, lambda=1160 para z=3)
## Z = 2
z=2
a = whittaker_GCV(x,z,m,u)
a[a[2]==min(a[2])]

## Z = 3
z=3
b = whittaker_GCV(x,z,m,u)
b[b[2]==min(b[2])]



# Implemento la obtencion de lambda optimo para mis datos 
# Para z=2
z=1
lambda_optimo_z1 = whittaker_GCV(x,z,m,u)
z=2
lambda_optimo_z2 = whittaker_GCV(x,z,m,u)
z=3
lambda_optimo_z3 = whittaker_GCV(x,z,m,u)
z=4
lambda_optimo_z4 = whittaker_GCV(x,z,m,u)


# Vemos cual es el valor optimo para distintos valores de z 
lambda_optimo_z1[lambda_optimo_z1[2]==min(lambda_optimo_z1[2])]
lambda_optimo_z2[lambda_optimo_z2[2]==min(lambda_optimo_z2[2])]
lambda_optimo_z3[lambda_optimo_z3[2]==min(lambda_optimo_z3[2])]
lambda_optimo_z4[lambda_optimo_z4[2]==min(lambda_optimo_z4[2])][1]


resultados = c()
for(i in 1:19){
  tabla = whittaker_GCV(x,i,m,u)
  lambda_estrella = tabla[tabla[2]==min(tabla[2])][1]
  gcv_estrella = tabla[tabla[2]==min(tabla[2])][2] 
  resultados = c(resultados, i, lambda_estrella, gcv_estrella)
}

# Valores de lambda y gcv para cada z 
resultados_z_gcv = data.frame(matrix(data=resultados, ncol=3, byrow=TRUE))
colnames(resultados_z_gcv) = c('z', 'lambda', 'GCV ')

write.csv(resultados_z_gcv, '/Users/mac/Facultad/Analisis Numerico/TP4/tabla_z_gcv.csv')


# Graficamos GCV(lambda)
# Para z=1
x_plot = lambda_optimo_z1[ ,'lambda']
y_plot = lambda_optimo_z1[ ,'GCV']
plot(x_plot,y_plot, type="b",pch='o', col="black",
     xlab='lambda', ylab='GCV', sub='z=1')
title('GCV en función de lambda',
      cex.main = 1.5,  font.main= 2, col.main= "black")

# Para z=2
x_plot = lambda_optimo_z2[ ,'lambda']
y_plot = lambda_optimo_z2[ ,'GCV']
plot(x_plot,y_plot, type="b",pch='o', col="black",
     xlab='lambda', ylab='GCV', sub='z=2', xlim = c(1,2000))
points(c(25), c(2.661766),col='3',pch=16,cex=2)
title('GCV en función de lambda',
      cex.main = 1.5,  font.main= 2, col.main= "black")

# Para z=3
x_plot = lambda_optimo_z3[ ,'lambda']
y_plot = lambda_optimo_z3[ ,'GCV']
plot(x_plot,y_plot, type="b",pch='o', col="black",
     xlab='lambda', ylab='GCV', sub='z=3')
points(c(602), c(2.579036),col='3',pch=16,cex=2)
title('GCV en función de lambda',cex.main = 1.5,  font.main= 2, col.main= "black")



# Ajustamienton con valores de lambda optimos
h1 = 1
h2 = 25
h3 = 602

tabla_resultados_2 = whit(x,m,u,h1,h2,h3)

# Calculo el RMSE tabla result 2
rmse(tabla_resultados_2$vi, tabla_resultados_2$u)


write.csv(tabla_resultados_2, '/Users/mac/Facultad/Analisis Numerico/TP4/tabla_2.csv')




# Verifico con datos de nocon y scott
u2 = c(12.02,10.87,12.56,16.47,17.62,17.13,21.25,22.14,23.6,22.27,24.47,23.53,23.02,23.02,26.97,23.78,21.41,19.61,20.91,23.13)
length(u2)
m =rep(1,20)
x = seq(1,20)
a = whittaker_GCV(x,2,m,u2)

x_plot = a[ ,'lambda']
y_plot = a[ ,'GCV']

plot(x_plot,y_plot, type="b",pch='o', col="blue",
     xlab='lambda', ylab='GCV')
title('GCV en función de lambda',
      cex.main = 1.5,  font.main= 2, col.main= "black")



a[a[2]==min(a[2])]








