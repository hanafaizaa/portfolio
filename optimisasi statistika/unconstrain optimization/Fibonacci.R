# Fungsi Fibonacci 

fibonacci <- function(n) {
  fn <- c(1,1)
  for (i in 2:n+1)
  {
    fn <- c(fn, (fn[i-1]+fn[i-2]))
  }
  return(fn)
}

#fibonacci(10)

#N <- 16

#data <- data.frame(
#  n = 1:N,
#  Fibonacci = fibonacci(15)
#)
#data

# Fibonacci Search Method

fib_search <- function(f, xl, xr, n){
  F = fibonacci(n) # Call the fibonnaci number function
  L0 = xr - xl # Initial interval of uncertainty
  R1 = L0 # Initial Reduction Ratio
  Li = (F[n-2]/F[n])*L0 
  
  R = Li/L0
  
  for (i in 2:n)
  {
    if (Li > L0/2) {
      x1 = xr - Li
      x2 = xl + Li
    } else {
      x1 = xl + Li
      x2 = xr - Li
    }
    f1 = f(x1)
    f2 = f(x2)
    
    if (f1 < f2) {
      xr = x2
      Li = (F[n - i]/F[n - (i - 2)])*L0 # New interval of uncertainty
    } else if (f1 > f2) {
      xl = x1
      Li = (F[n - i]/F[n - (i - 2)])*L0 # New interval of uncertainty
    } else {
      xl = x1
      xr = x2
      Li = (F[n - i]/F[n - (i - 2)])*(xr - xl) # New interval of uncertainty
    }
    L0 = xr - xl
    R = c(R, Li/R1)
  }
  
  list1 <- list(x1, f(x1), R) # Membuat sebagai list sehingga bisa dipanggil keluar dengan fungsi return.
  names(list1) <- c("x", "f(x)", "R") # Memberi nama pada elemen suatu list.
  list2 <- list(x2, f(x2), R)
  names(list2) <- c("x", "f(x)", "R")
  if (f1 <= f2) {
    return(list1) # Final result
  } else {
    return(list2) # Final result 
  }
}

f <- function(x) {
  -(1/(x-1)^2)*(log(x)-2*(x-1)/(x+1))
}  ###bisa diganti

Fib = fib_search(f, 1.5, 4.5, 70) ##bisa diganti
Fib


curve(f,xlim=c(-3,3), col='steelblue',lwd=2)
abline(h=0)
abline(v=0)
