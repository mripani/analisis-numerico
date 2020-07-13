##################### Sumación Númerica ##################### 
######################## Lubbock ############################


#Argumentos:
#a=índice del primer punto. 
#n=intervalos incluidos en la sumatoria.
#m=subperíodos.
#o=opción, para fórmula únicamente con diferencias o=1 y o=2 para fórmula con diferencias y nablas.


x<-seq(0,50,by=5)
fx<-c(3,28,103,228,403,628,903,1228,1603,2028,2503)

A<-cbind(x,fx)

Lubbock<-function(A,a,n,m,o){
  Suma=m*sum(A[a:(a+n-1),2])
  A1<-c((m-1)/2,((m^2)-1)/-(12*m),((m^2)-1)/(24*m),(((m^2)-1)*(19*(m^2)-1))/-(720*(m^3)),(((m^2)-1)*(9*(m^2)-1))/(480*(m^3)))           
  x<-A[,1]                                          
  fx<-A[,2]
  l<-length(x)
  diferencia <-matrix(0,l,5)
  diferencia[,1]<-fx
  for (j in 2:5) {
    for (i in 1:(l-j+1)){
      diferencia[i,j] <-(-diferencia[i,j-1]+diferencia[i+1,j-1])
    }
  }
  c=diferencia[a,1:5]
  Sumatoria=Suma
  if(o==2){                          #Sumatoria con nablas y diferencias.
    no=rep(0,5)
    k=5
    if((n+1)<5){
      k=n+1
    }
    for(j in 1:k){
      no[j]=diferencia[(a+n-j+1),j]
    }
   for(h in 1:5){  
    if(h==3|h==5){
     Sumatoria=Sumatoria+(-A1[h]*(no[h]+c[h]))
    }
     else
       Sumatoria=Sumatoria+(A1[h]*(no[h]-c[h]))
   }
  }
  else
    if(o==1){                                     #Sumatoria únicamente con diferencias.
    if(length(A[a:nrow(A),1])<(5+n)){
     return("No es posible realizar la sumatoria únicamente con diferencias")
    }
      else 
        no=diferencia[a:a+n,]
      Sumatoria=sum(A1*(no-c))+Sumatoria
      
    }
  return(Sumatoria)
}
  
#Ejemplos:
Lubbock(A,5,2,6,1)
Lubbock(A,2,5,5,2)
Lubbock(A,1,4,5,2)
Lubbock(A,1,6,5,1)

