#####################################################################################################
#############################  Resolucisn de Sumas Subperisdicas  ###################################
#############################       Fsrmula de Woolhouse          ###################################
#####################################################################################################

#Se debe gerenar una funcisn "func" con la ecuacisn deseada.
#UTILIZAR EL COMANDO EXPRESSION(funcion que se desee integrar)
#NO FUNCIONA CON 3X,UTILIZAR 3*X
#Ej  func=expression("Funcisn deseada")

func=expression(1/(x+2))
m<-c(0,7,14)
n<-c(0.5,0.111111,0.0625)
c<-c(1:length(m))
Datos<-cbind(c,m,n)
#Se crea la funcion "woolhouse" con argumentos:
#n: Cantidad de periodos
#m: Cantidad de subperiodos
#u: Cantidad de terminos de Bernoulli a utilizar.Por defecto u=3.Maximo u=11

Woolhouse=function(Datos,a,n,m,u=3){
  d<-c(Datos[,3])
  
  #SEPARAREMOS LA FSRMULA EN 3 PARTES
  
  #PRIMERA PARTE
  sum=0
  for (i in 1:n) {
    I=d[a+i]
    sum=sum+(m*I)
  }
  print('Termino primera parte')
  #SEGUNDA PARTE
  
  x=c(1,m*n)
  fx=eval(func)
  fx2 = sum(eval(fx[2]))
  fx1 = sum(eval(fx[1]))
  sum1=-(m-1)/2*(fx2-fx1)
  print('Termino segunda parte')
  #TERCERA PARTE
  
  #Generamos un vector con los nzmeros de bernoulli a utilizar a partir de b2
  B=c(1,-1/2,1/6,0,-1/30,0,1/42,0,-1/30,0,5/66,0,-691/2730,0,7/6,0,-3617/510,0,43867/798,0,-174611/330)
  be=c()
  for (i in 1:u) {
    be=c(be,B[2*i+1]/factorial(2*i))
  }
  
  #Realizamos las derivadas necesarias
  dfunc=eval(func)
  for (j in 1:(2*u-3)) {
    dfunc=c(dfunc,D(dfunc[j],"x"))
  }
  
  sum2=0
  for (s in 1:(u-1)) {
    fx=eval(dfunc[2*s])
    sum2=sum2-be[s]*(m^(2*s)-1)*(fx[2]-fx[1])
  }
  print('Termino tercera parte')
  #Juntamos los resultados que hemos obtenido y resolvemos la ecuacisn de Woolhouse
  
  y=sum+sum1+sum2
  return(y)
}

