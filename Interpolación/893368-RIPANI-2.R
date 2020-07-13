######## Análisis Númerico
####### Trabajo Práctico 2
# Máximo Ripani



## Preparo los datos

# Cantidad de digitos a utilizar
options(digits=15)


library(readxl)
Datos_TP2 = read_excel("~/Documents/Facultad/Analisis Numerico/TP2/Datos TP2.xlsx")

# Defino la parte de los datos para el registro terminado en 8 
df_part = Datos_TP2[161:180,]

# Asigno 'Nro de Orden' a mis X (20)
x =as.numeric(unlist(df_part[,2]))

# Asigno Fx
fx = as.numeric(unlist(df_part[,4]))

# Armo la tabla
A=cbind(x,fx)

# Valores a interpolar (81)
total = c(801:901)
xs = total[!total %in% x]

## Ejercicio 1. 

# Funcion interpoladora de Newton
Newton<-function(A,xs){
        b<-nrow(A)-1    #este fragmento del codigo se corresponde al calculo de las diferencias div.
        mm<-A[,1]  
        m<-A[,2]        
        #  M<-(mm[length(mm)]-mm[1])/900   #El intervalo es dividido en 900, valor que se utiliza para establecer el paso entre 
        #  B<-seq(mm[1],mm[length(mm)],M)  #los valores sobre los cuales se interpolar? para graficar. 
        #  names<-c("x","f(x)")
        k<-nrow(A)-1
        for(i in 1:k){
                a<-rep(0,nrow(A)) 
                for(j in 1:b){  
                        a[j]<-(m[j+1]-m[j])/(mm[j+i]-mm[j])  
                }
                A<-cbind(A,a) 
                b<-b-1 
                m<-A[,2+i] 
        }
        #  vint<-c()  #se genera el vector en el cual se guardan las interpolaciones
        #  for(h in 1:length(B)){  #para cada uno de los valores del vector B...
        #    interpolador<-A[1,2]  #defino el primer termino del polinomio interpolador (com?n a todas las interpolaciones de estos datos)
        #    for(f in 1:k){  #para cada termino del polinomio interpolador...
        #      u<-1  
        #      for(p in 1:f){  #observar que el n?mero de t?rmino del polinomio interp. coincide con la 
        #        #cantidad de terminos a multiplicar (terminos que contienen (x-Xi))
        #        u<-u*(B[h]-A[p,1]) 
        #      }
        #      interpolador=interpolador+(A[1,f+2])*u  #armo el valor de la interpolacion como la suma de cada uno de los terminos calculados
        #    }
        #    vint<-c(vint,interpolador) #guardo el valor interpolado en el vector auxiliar
        #  }
        
        #  Ahora se interpola el punto especificado. Lo anterior es meramente para graficar
        
        interpolador=A[1,2]  
        for(f in 1:k){  
                u<-1  
                for(p in 1:f){  
                        u<-u*(xs-A[p,1]) 
                }
                interpolador<-interpolador+(A[1,f+2])*u
        }
        coef=round(as.vector(A[1,2:ncol(A)]),2)
        j=("(x-a)")
        e=c("1")
        u=c(coef[1])
        for(c in 2:length(coef)){
                a=x[c-1]
                e=paste(c(e,gsub("a",a,j)),collapse ="*")
                u=c(u,paste(c(e,coef[c]),collapse = "*"))
        }
        e=parse(text=(paste(u,collapse="+")))
        imp<-interpolador
        names(imp)<-c("f(xs)")
        names(e)<-c("f(x)")
        #  print(e)
        #  print(imp)
        #  dev.new()
        # plot(B,vint,type = "l",xlab = "X",ylab = "Fx",col="black",lwd=3,main = "Polinomio Newton")
        # points(A,pch = 21,cex=1.5,lwd=0.5,bg="red")
        # points(xs,imp,pch = 21,cex=1.8,lwd=0.9,bg="yellow")
        
        # Aqui agrego para que me de el vector interpolado
        names(imp)=NULL
        return(imp)
}


# Interpolacion metodo de Newton
newton = Newton(A, xs)
plot(xs,newton, type="b",pch='*', col="blue",
     xlab='X', ylab='F(X)')
title('Interpolación método de Newton',
      cex.main = 1.5,  font.main= 2, col.main= "black")
legend(850,-10000, legend=c('Newton'), col=c('blue'), pch=c('*'), lty=c(2), ncol=1)



## Ejercicio 2. 

# Funcion interpoladora de Spline
spline_LG=function(datos,xs){
        x=datos[,1]
        fx=datos[,2]
        n=nrow(datos)-1
        a=fx
        h=matrix(0,n,1)
        #   VECTOR=seq(x[1],x[length(x)],0.001)
        VECTOR=xs
        VECTOR2=c()
        
        for (i in 1:n) {
                h[i]=(x[i+1]-x[i])}
        A=matrix(0,n+1,n+1)
        A[1,1]=1
        A[n+1,n+1]=1
        F=(matrix(0,n+1,1))
        
        for (i in 2:n) {
                A[i,i-1]=h[i-1]
                A[i,i]=2*(h[i-1]+h[i])
                A[i,i+1]=h[i]
                F[i]=3*((a[i+1]-a[i])/h[i])-3*((a[i]-a[i-1])/h[i-1])}
        
        c=solve(A)%*%F
        c[is.nan(c)]=0
        
        d=matrix(0,n,1)
        b=matrix(0,n,1)
        for (i in 1:n) {
                d[i]=(c[i+1]-c[i])/(3*h[i])
                b[i]=(a[i+1]-a[i])/h[i]-h[i]*(2*c[i]+c[i+1])/3 }
        
        
        for (r in 1:length(VECTOR)) {
                for (i in 1:n) {
                        if (x[i]<=VECTOR[r] & x[i+1]>=VECTOR[r]){
                                s=i }}
                
                rs=a[s]+b[s]*(VECTOR[r]-x[s])+c[s]*(VECTOR[r]-x[s])^2+d[s]*(VECTOR[r]-x[s])^3
                coef=matrix(c(a[1:n],b,c[1:n],d),ncol = 4,byrow = FALSE)
                
                VECTOR2=c(VECTOR2,rs)
        }
        #    for (i in 1:n) {
        #      if (x[i]<=xs & x[i+1]>=xs){
        #        s=i }}
        
        #    ys=a[s]+b[s]*(xs-x[s])+c[s]*(xs-x[s])^2+d[s]*(xs-x[s])^3
        #    Y=c(ys,a[s],b[s],c[s],d[s])
        #    names(Y)=c("X","a","b","c","d")
        #    print(Y)
        #    dev.new()
        #    plot(VECTOR,VECTOR2,type = "l",xlab = "X",ylab = "Fx",col="red",lwd=3,main = "Splines L") 
        #    points(x,fx,pch = 21,cex=1.1,lwd=0.5,bg="black")
        #    points(xs,ys,pch = 21,cex=1.5,lwd=0.5,bg="blue")
        return(VECTOR2)
}

# Interpolacion metodo Spline
spline = spline_LG(A, xs)
plot(xs,spline,type='b',pch='o', col='yellow', xlab='X', ylab='F(x)')
title('Interpolación método de Splines',
      cex.main = 1.5,  font.main= 2, col.main= "black")
legend(850,14000, legend=c('Splines'), col=c('yellow'), pch=c('o'), lty=c(1), ncol=1)



## Graficamos Spline y Newton juntos 

plot(xs,spline,type='o' , pch='o', lty=1 ,col='yellow', xlab = 'x', ylab= 'F(x)', ylim=c(0,20000))
title('Comparación de ambos métodos',
      cex.main = 1.5,  font.main= 2, col.main= "black")
lines(xs, newton, lty=2, col='blue')
points(xs, newton, pch='*', col='blue')
legend(850,8500, legend=c('Spline', 'Newton'), col=c('yellow', 'blue'), pch=c('o', '*'), lty=c(1,2), ncol=1)


## Graficamos observador vs interpolados
# Spline
plot(xs,spline,type='l', col='orange', xlab='X', ylab='F(x)')
title('Interpolación método de Splines',
      sub='Frontera libre',
      cex.main = 1.5,  font.main= 2, col.main= "black")
points(xs, f_obs,pch='o')
legend(850,14000, legend=c('Spline','Observados'), col=c('orange','black'), pch=c('o','*'), lty=c(1), ncol=1)

#Newton
plot(xs,newton,type='l', col='blue', xlab='X', ylab='F(x)',ylim=c(0,50000))
title('Interpolación método de Newton',
      cex.main = 1.5,  font.main= 2, col.main= "black")
points(xs, f_obs,pch='o')
legend(830,48000, legend=c('Spline','Observados'), col=c('blue','black'), pch=c('o'), lty=c(1), ncol=1)



## Ejercicio 3.

# RMSE función #####(chequear cual de las dos esta bien)
rmse = function(m, o){
        sqrt(mean((m - o)^2))
}

# Datos con los valores observados 
data2<- read_excel("~/Documents/Facultad/Analisis Numerico/TP2/Datos TP2.xlsx",
                   sheet=2)
df_2_part = data2[801:901,]
df_viernes_obs = subset(df_2_part, df_2_part[,4] != 5)


# F(x) observados
f_obs = as.numeric(unlist(df_viernes_obs[,3]))


# RMSE Newton
rmse_newton = rmse(newton,f_obs)

# RMSE Splines
rmse_spline = rmse(spline, f_obs)


## Ejercicio 4

# Intervalos de 2 a 20
intervalos = c(2:20)
resultados = c()
for(i in intervalos){
        new_x = x[1:i]
        new_fx = fx[1:i]
        new_A = A[1:i, ]
        inter_newton = Newton(new_A, xs)
        new_rmse = rmse(inter_newton, f_obs)
        resultados = c(resultados,i ,new_rmse)
}
matriz_resultados = matrix(data=resultados, ncol=2, byrow=TRUE)

plot(matriz_resultados2[,1],matriz_resultados2[,2],type='b',ylab='RMSE',xlab='Tamaño de intervalo', col='orange')
title('Fenómeno de Rounge',cex.main = 1.5,  font.main= 2, col.main= "black")


## Ejercicio 5 

# Cambio el orden de los datos

# Desordeno el df
set.seed(42)
rows = sample(nrow(df_part))
df_desordenado = df_part[rows, ]

# Asigno X (20)
x_desorden =as.numeric(unlist(df_desordenado[,2]))
# Asigno Fx
fx_desorden = as.numeric(unlist(df_desordenado[,4]))
# Armo la tabla con valores desordenados
A_desorden=cbind(x_desorden,fx_desorden)


# Interpolacion metodo de Newton valores desordenados
newton_desordenado = Newton(A_desorden, xs)
plot(xs,newton, type="b",pch='o', col="orange",
     xlab='X', ylab='F(X)')
title('Interpolación método de Newton',
      cex.main = 1.5,  font.main= 2, col.main= "black",
      sub='Puntos dato desordenados', cex.main=1, col.sub='black')
legend(850,-10000, legend=c('Newton'), col=c('orange'), pch=c('^'), lty=c(2), ncol=1)


# RMSE Newton desordenados
rmse_newton_desorden = rmse(newton_desordenado, f_obs)





## Ejercicio 6 

# Interpolacion con todos los valores de A para intervalos de 9 XS
intervalos2 = c(1,seq(9,81,9))
resultad = c()
for(i in 1:9){
  inter = Newton(A, xs[intervalos2[i]:intervalos2[i+1]])
  rms = rmse(inter, f_obs[intervalos2[i]:intervalos2[i+1]])
  resultad = c(resultad, i, rms)
}
resultadosh = matrix(data=resultad, ncol=2, byrow=TRUE)
plot(resultadosh[,1],resultadosh[,2])


# Interpolacion con 2 valores para todos los xs y promedio 
secu = seq(0,20,2)
vals = c()
metricas = c()
for(i in 1:19){
  val2 = i+1
  As = A[i:val2, ]
  inter = Newton(As,xs)
  vals = c(vals, inter)
  metric = rmse(inter,f_obs)
  metricas = c(metricas, metric)
}

matriz_resultadok = matrix(data=vals, ncol=19, nrow=81)


# Vemos como le fue a las metricas
plot(1:19, metricas,type='l')
title('Métricas de interolaciones con 2 valores dato')
# Calculo promedio de todas las interoplaciones 
promedios = c()
for(i in 1:81){
  promedios = c(promedios,mean(matriz_resultadok[i,]))
}

# RMSE 
rmse_promedios = rmse(promedios, f_obs)




