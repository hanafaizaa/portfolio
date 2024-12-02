# Metode Tertutup: Metode Biseksi
root_bisection <- function(f, a, b, tol=1e-7, N=100){
  iter <- 0
  fa <- f(a)
  fb <- f(b)
  
  while(abs(b-a)>tol){
    iter <- iter+1
    if(iter>N){
      warning("iterations maximum exceeded")
      break
    }
    x <- (a+b)/2
    fx <- f(x)
    if(fa*fx>0){
      a <- x
      fa <- fx
    } else{
      b <- x
      fb <- fx
    }
  }
  
  # iterasi nilai x sebagai return value
  root <- (a+b)/2
  return(list(`function`=f, root=root, iter=iter))
}

#Contoh penggunaan
# Carilah akar persamaan f(x) = x*e^(-x) + 1 pada rentang x = [-1,0]
# dengan nilai toleransi sebesar 10^(-7)

root_bisection(function(x){x*exp(-x)+1},
               a=-1, b=0)

#plot
f=function(x){x*exp(-x)+1}
curve(f, from = -1, to = 0)
abline(h = 0, col="red", lty=2)


root_bisection(function(x){x^4 - x^3 + 2*x^2 -2*x -12},
               a=-2, b=0)

#plot
f=function(x){x*exp(-x)+1}
curve(f, from = -1, to = 0)
abline(h = 0, col="red", lty=2)