############################################################################################
#############################  Resolución de Ecuaciones  ###################################
#############################      Método de Simpson     ###################################
############################################################################################


#Se debe generar una función "func" con la ecuación deseada.

func<-function(x){
  y=x^2-2
  return(y)
}

#Se crea la función "simpson" con argumentos:
#a: Limite Inferior de la integral
#b: Limite Superior de la integral
#n: Tamaño de cada intervalo

simpson<-function(a,b,n) {
  
  if ((n/2)!=round((n/2))) {
    return("n debe ser par")}

  else {
    h = (b-a)/n

#Se arma una secuencia que tome valores impares de i desde 1 hasta n-1

    if(n>2) im= seq(1, n-1, by=2) else im=1 

#Se arma una secuencia que este conformada por los valores pares de i desde 2 hasta n-2

    if(n>2) pa= seq(2, n-2, by =2) else pa=0  

#Se hace la suma de todos los valores que toma la funcion en a+ih siendo i impar 

    suma_impar=sum(func((a+h*im)))

#Si n es mayor a 2 se hace la suma de los valores que toma la funcion en todos los valores de i pares.
    
    if(n>2) p=sum(func((a+h*pa))) else p=0       
  
#Reemplazo en la funcion de Simpson.    
    
    integral=(h/3)*(func(a)+func(b)+(4*suma_impar)+(2*p))
    
    return(integral)}
}
