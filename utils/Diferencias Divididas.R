m<-c(1,2,3,4,5,6,7)
n<-c(144,56,35,22,78,3,17)
A<-cbind(m,n)

#La funcion requiere una matriz "A" de dos columnas con los valores de x (primera col) y f(x) (segunda col)


diferenciasdivididas<-function(A){
  m<-A[,1]
  n<-A[,2]
  lm<-length(m)
  P<-cbind(m,n)
  names<-c("x","f(x)")
  for(b in 1:(lm-1)){ #la accion se repite tantas veces como orden de diferencias divididas se busque
    a=n #a es un vector auxiliar
    for(j in 1:b){ #la metodologia en que trabaja esta funcion es identica a la explicada en el script
      #de Diferencias Simples. Unicamente cambia el calculo de la diferencia en si, es decir el valor que se
      #grava en el objeto "w". 
      ln<-length(a)
      z=length(a)-1
      w<-rep(0,z)
      for(i in 1:z){
        if(j==1){
          w[i]=(n[i+1]-n[i])/(m[i+1]-m[i])
        }else{
          w[i]=(a[i+1]-a[i])/(m[i+j]-m[i])
        }
      }
      a=w
    }
    d<-a
    f<-c(d,rep(0,b))
    P=cbind(P,f) #en cada iteracion se "pega" el resultado (correspondiente a la diferencia dividida) a los resultados anteriores
    u<-"Dif"
    uu<-b
    name<-paste (u,uu, sep = "", collapse = "")
    names<-c(names,name)
  }
  colnames(P)<-names
  #print(P)
  return(P)
}

diferenciasdivididas(A)

