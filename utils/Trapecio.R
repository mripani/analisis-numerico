############################################################################################
#############################    Integración Numérica    ###################################
#############################     Método del Trapecio    ###################################
############################################################################################


#Se debe generar una función "func" con la ecuación deseada.

func=function(x){ y= 3*x
    return(y)}

#Se crea la función "trapecio" con argumentos:
#a: Limite Inferior de la integral
#b: Limite Superior de la integral
#n: Tamaño de cada intervalo

trapecio<-function(a,b,n){

  h =(b-a)/n

#Secuencia de 1 a n-1.

  if(n>1){ c=c(1:(n-1))} 
  else c=0   #Caso n==1     
  
#Reemplazo en la funcion de trapecios.  
  
  integral=(h/2)*(func(a)+func(b)+(2*sum(func(a+(c*h)))))    
  
  return(integral) }

trapecio(0,2,6)