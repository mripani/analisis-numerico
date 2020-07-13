#####################################################################################################
#############################         Integración Numérica        ###################################
#############################      Fórmula de Euler-Mac Laurin      ###################################
#####################################################################################################

#Se debe gerenar una función "func" con la ecuación deseada.
#UTILIZAR EL COMANDO EXPRESSION(funcion que se desee integrar)
#NO FUNCIONA CON 3X,UTILIZAR 3*X
#USAR X MAYÚSCULA COMO VARIABLE
#Ej  func=expression((Función deseada))

func=expression(((exp(1))^(X-5)))

#li=Limite superior de la integral (ACEPTA NO ENTEROS)
#ls=limite inferior de la integral (ACEPTA NO ENTEROS)
#ti=Limite superior de la transformación en caso de límites no enteros en caso que no se quiera redondear
#ts=limite inferior de la transformación en caso de límites no enteros en caso que no se quiera redondear
#u=Cant números de bernoulli con un máximo de u=11,Por defecto u=3

eulermaclaurint=function(li,ls,ti=round(li),ts=round(ls),u=3){
  
  #Primero comprobamos que sean enteros los limites de la integral.
  #Caso contrario realizamos una transformación y los extremos son [0,1]
 
 if (ls==round(ls) & li==round(li)) {
 }
 else{
  transf=solve(matrix(c(1,1,li,ls),2),c(ti,ts))
  li=ti
  ls=ts
  a=transf[1]
  b=transf[2]
  func=gsub("X","((X-a)/b)",func)  #Reemplazamos la variable 
  func=paste((1/b),func,sep="*")    #Agregamos el coeficiente correspondiente del diferencial
  func=parse(text=func)
 }

#Dividimos la fórmula en 3 partes   
  
#Primera Parte

 X=seq(from=li,to=ls-1) #Desde li hasta (ls-1)
 fx=eval(func)
 sum1=sum(fx)
 X=c(li,ls)
  
#Segunda Parte
 fx=eval(func)
 sum2=(fx[2]-fx[1])/2
 
#Tercera Parte 
    
#Generamos un vector con los números de bernoulli a utilizar a partir de b2
 B=c(1,-1/2,1/6,0,-1/30,0,1/42,0,-1/30,0,5/66,0,-691/2730,0,7/6,0,-3617/510,0,43867/798,0,-174611/330)
 be=c()
 for (i in 1:(u-1)) {
  be=c(be,B[2*i+1]/factorial(2*i))
 }

#Realizamos las derivadas necesarias
 dfunc=func
 for (j in 1:(2*u-3)) {
  dfunc=c(dfunc,D(dfunc[j],"X"))
 }

 sum3=0
 for (s in 1:(u-1)) {
  fx=eval(dfunc[2*s])
  if (length(fx)==2 ){   #En caso de una derivada no depender de "x",el vector x deja de ser vector
    sum3=sum3+be[s]*(fx[2]-fx[1])
  }
 }

#Juntamos los resultados que hemos obtenido y resolvemos la ecuación de Euler-Mac Laurin
 
#Usar esto y agregar delante de la linea de abajo "#" para tener el resultado desagregado en las tres partes
#y=c(sum1,sum2,sum3)
y=sum1+sum2-sum3
 print(y)
}
