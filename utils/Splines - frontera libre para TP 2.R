##EJEMPLO DATOS

 m<-c(1,2,3,4,5,6,7)
 n<-c(144,56,35,22,78,3,17)
 A=cbind(m,n)
 
 # Valores a interpolar
 xs=seq(1,7,0.2)

#Los argumentos de la función son de una matriz "datos" que incluya dos columnas.
#Una columna con valores de x ,y otra columna con sus respectivos f(x) 
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
#Como resultado nos genera un vector en el cual nos da, para el x elegido,
#su fx seguido de los coeficientes del spline.
#También nos permite ver la gráfica de la funcion interpolante obtenida y la ubicacion en ella
#Para el punto requerido.

pxs=  spline_LG(A,xs)

# prepara los márgenes para que quepa el gráfico
par("mar"=c(1,1,1,1))
plot(xs,pxs, type="line", col="red")


 