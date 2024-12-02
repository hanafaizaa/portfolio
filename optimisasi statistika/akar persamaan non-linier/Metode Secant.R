# Metode Terbuka: Secant
root_secant <- function(f, x, tol=1e-7, N=100){
  iter <- 0
  
  xold <- x
  fxold <- f(x)
  x <- xold+10*tol
  
  while(abs(x-xold)>tol){
    iter <- iter+1
    if(iter>N)
      stop("No solutions found")
    
    fx <- f(x)
    xnew <- x - fx*((x-xold)/(fx-fxold))
    xold <- x
    fxold <- fx
    x <- xnew
  }
  
  root<-xnew
  return(list(`function`=f, root=root, iter=iter))
}

#contoh penerapan:
result_secant <- root_secant(function(x){x-exp(-x)}, x=0)
result_secant

result_secant <- root_secant(function(x){x^3 +x^2 -3*x - 3}, x=1)
result_secant


#plot
f=function(x){x - exp(-x)}
curve(f, from = 0, to = 1)
abline(h = 0, col="red", lty=2)
abline(v = 0.567, col="blue", lty=2)


f=function(x){x^3 +x^2 -3*x - 3}
curve(f, from = 0, to = 3)
abline(h = 0, col="red", lty=2)
abline(v = 1.73205, col="blue", lty=2)
