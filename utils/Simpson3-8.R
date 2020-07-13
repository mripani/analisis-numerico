############################################################################################
#############################  Resolución de Ecuaciones  ###################################
#############################   Método de Simpson 3/8    ###################################
############################################################################################


#Se debe generar una función "func" con la ecuación deseada.

func<-function(x){
  y=x^2-2
  return(y)
}

#Se crea la función "simpson3_8" con argumentos:
#a: Limite Inferior de la integral
#b: Limite Superior de la integral
#n: Tamaño de cada intervalo

SIMPSON3_8<-function(a,b,n){
  
  if ((n/3)!=round((n/3))) return("n debe ser múltiplo de 3")
  else {
    h = (b-a)/n
    
#Armo dos secuencias que combinadas sean todos lo no multiplos de 3    
  
    if(n>3) m1=seq(1,n-1,3) else m1=1 
    if (n>3) m2=seq(2,n-1,3) else m2=2
    
# todos los numeros multiplos de 3 ,desde 1 hasta n-3  
    
    if (n>3) m3= seq(3, n-3, by = 3) else m3=0 
    
#Sumatoria de todos los valores que toma la funcion en a+ih siendo i no múltiplo de 3    
    
    M1=sum(func(a+(h*m1)))      
    M2=sum(func(a+(h*m2)))   
    
#Sumatoria de todos los valores que toma la funcion en a+ih siendo i  multiplo de 3
  
    if (n>3)M3=sum(func(a+(h*m3))) else M3=0 
    
#Reemplazo en la funcion de Simpson.        
    
    integral= ((3/8)*h)*(func(a)+func(b)+(3*M1)+(2*M3)+(3*M2))
    
    return(integral)
  }
}
