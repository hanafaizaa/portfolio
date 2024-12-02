#Metode Terbuka: Newton Raphson
root_newton <- function(f, fp, x0, tol=1e-7, N=100){
  iter <- 0
  xold<-x0
  xnew <- xold + 10*tol
  
  while(abs(xnew-xold)>tol){
    iter <- iter+1
    if(iter>N){
      stop("No solutions found")
    }
    xold<-xnew
    xnew <- xold - f(xold)/fp(xold)  
  }
  
  root<-xnew
  return(list(`function`=f, root=root, iter=iter))
}

#Selesaikan persamaan non-linier x- e^(-x) = 0 dengan metode Newton Raphson
result_newraph <- root_newton(function(x){x-exp(-x)},
            function(x){1+exp(-x)},
            x0=0)
result_newraph

#plot
f=function(x){x - exp(-x)}
curve(f, from = 0, to = 1)
abline(h = 0, col="red", lty=2)
abline(v = 0.567, col="blue", lty=2)

result_newraph <- root_newton(function(x){x^3 +x^2 -3*x - 3},
                              function(x){3*x^2 + 2*x - 3},
                              x0=1)
result_newraph

