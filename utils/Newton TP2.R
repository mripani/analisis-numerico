 #x<-c(1,2,3,4,5,6,7)
 #fx<-c(144,56,35,22,78,3,17)
 #A=cbind(x,fx)
 
 # Valores a interpolar
 #xs=seq(1,7,0.2)

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

#Ejemplo
#pxs=  Newton(A,xs)

#plot(xs,pxs, type="line", col="red")
